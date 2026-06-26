
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sentiner

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/thieled/sentiner/graph/badge.svg)](https://app.codecov.io/gh/thieled/sentiner)
<!-- badges: end -->

The goal of sentiner is to provide a versatile, fully reporducible, and
relatively lightweight, package to conduct **targeted sentiment
analyses** in R.

The package implements a modular pipeline to **(1)** identify **named
entities** in text using
[GliNER](https://huggingface.co/urchade/gliner_large-v2), **(2)**
**link** and filter extracted entities to a set of known entities using
exact and fuzzy matching by string similarity, **(3) machine-translate**
text while preserving original entity names using
[easieRnmt](https://github.com/thieled/easieRnmt) and **(4) classify
targeted sentiment** (positive/neutral/negative) toward extracted
entities using a fine-tuned version of a [BERT-NLI
model](https://huggingface.co/MoritzLaurer/deberta-v3-base-zeroshot-v2.0).

``` mermaid
flowchart LR
    A["📄 Text"] --> B["Preprocessing<br/>(Sentence Tokenization)"]
    B --> C["NER<br/>(GLiNER)"]
    C -.-> D["Entity Linking<br/>(Fuzzy-Match)"]
    D -.-> E["Masked<br/>Translation<br/>(EasyNMT)"]
    C ---> F
    E --> F["Targeted<br/>Sentiment<br/>(NLI)"]

    style D stroke-dasharray: 5 5
    style E stroke-dasharray: 5 5
```

## Installation

You can install the development version of sentiner from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thieled/sentiner")
```

## Example

#### Setup

First, you must set up the python backend of sentiner. This
automatically detects your CUDA GPU if available and installs the
corresponding versions of torch and gliner. Thanks to
[JBGruber](https://github.com/JBGruber), the python backend of the
package is now smoothly managed by `uv` via `reticulate`. You can
customize the directories used by `uv` for installing libraries
(`uv_cache_dir`) and the cache where models are downloaded to
(`models_dir`).

``` r
library(sentiner)
sentiner::initialize_sentiner()
```

#### Toy Data

Now, let’s create a toy example data. To simplify this example, we use
English-only text. However, sentiner is designed with multilingual
corpora in mind.

``` r

docs <- data.frame(
  doc_id = 1:5,
  text = c(
    "Honestly, the Republicans have no real plan. Just chaos and noise.",
    "Great leadership from DonaldJTrump – finally someone who gets things done.",
    "Nobody worked harder for working families than Hillary Clinton.",
    "The Democrats keep promising change but deliver nothing.",
    "Trump's trade war hurt American farmers. That's just a fact."
  )
)

docs
#>   doc_id
#> 1      1
#> 2      2
#> 3      3
#> 4      4
#> 5      5
#>                                                                         text
#> 1         Honestly, the Republicans have no real plan. Just chaos and noise.
#> 2 Great leadership from DonaldJTrump – finally someone who gets things done.
#> 3            Nobody worked harder for working families than Hillary Clinton.
#> 4                   The Democrats keep promising change but deliver nothing.
#> 5               Trump's trade war hurt American farmers. That's just a fact.
```

#### Cleaning and tokenization

Next, we clean and tokenize the text. The function also extracts the
context for each tokenized sentence (previous and next sentence).

``` r

cleaned_dt <- sentiner::clean_text(docs, 
                                   text_col = "text", 
                                   id_col = "doc_id", 
                                   group_col = "country")
#> Tokenizing long texts into sentences and chunks...

dplyr::glimpse(cleaned_dt)
#> Rows: 7
#> Columns: 7
#> $ sen_id             <chr> "1_1", "1_2", "2_1", "3_1", "4_1", "5_1", "5_2"
#> $ doc_idx            <int> 1, 1, 2, 3, 4, 5, 5
#> $ sen_idx            <int> 1, 2, 1, 1, 1, 1, 2
#> $ doc_id_u           <int> 1, 1, 2, 3, 4, 5, 5
#> $ text_orig          <chr> "Honestly, the Republicans have no real plan. Just …
#> $ text_clean         <chr> "Honestly, the Republicans have no real plan.", "Ju…
#> $ text_clean_context <chr> "Honestly, the Republicans have no real plan. Just …
```

#### NER extraction

Now we can call gliner to extract named entities:

``` r
gliner_dt <- sentiner::gliner_extract(cleaned_dt,
                       labels = c("political_party", "politician"))
#> ℹ Loading GLiNER model: urchade/gliner_multi-v2.1 on cpu
#> ℹ Extracting entities: 1 chunk(s). Please hang on...✔ Entity extraction completed in 1.7s               

dplyr::glimpse(gliner_dt)
#> Rows: 5
#> Columns: 14
#> $ id           <chr> "1_1_1", "2_1_1", "3_1_1", "4_1_1", "5_1_1"
#> $ sen_id       <chr> "1_1", "2_1", "3_1", "4_1", "5_1"
#> $ doc_idx      <int> 1, 2, 3, 4, 5
#> $ sen_idx      <int> 1, 1, 1, 1, 1
#> $ ent_idx      <int> 1, 1, 1, 1, 1
#> $ doc_id_u     <int> 1, 2, 3, 4, 5
#> $ text_clean   <chr> "Honestly, the Republicans have no real plan.", "Great le…
#> $ entity_name  <chr> "Republicans", "DonaldJTrump", "Hillary Clinton", "The De…
#> $ label        <chr> "political_party", "politician", "politician", "political…
#> $ score        <dbl> 0.9589791, 0.9351132, 0.9852717, 0.9478762, 0.9358057
#> $ start        <int> 15, 23, 48, 1, 1
#> $ end          <int> 26, 35, 63, 14, 6
#> $ ner_model    <chr> "urchade/gliner_multi-v2.1", "urchade/gliner_multi-v2.1",…
#> $ ner_detected <dttm> 2026-06-26 23:30:56, 2026-06-26 23:30:56, 2026-06-26 23:3…
```

#### Entity Linking (optional)

The NER output usually contains more entities than we are interested in.
So we want to narrow it down to a list of entities that we know. This
also allows us to merge entitiy IDs - which allows merging further data,
such as party-membership of politicians, e.g. We use a fuzzy-matching
approach to do this, allowing for a little leeway between the detected
and known names – but only minimally.

``` r

ner_reference <- data.frame(
  label = c("Republicans", "GOP",
            "Democrats", "Democratic Party",
            "Donald Trump", "Trump",
            "Hillary Clinton", "Hillary"),
  entity_id = c("us-o1", "us-o1",
                "us-o2", "us-o2",
                "us-i1", "us-i1",
                "us-i2", "us-i2")
)

f_match_ner <- digimodhelpers::fuzzy_match_df(input = gliner_dt, 
                                              input_col = "entity_name", 
                                              input_id_col = "id", 
                                              target = ner_reference, 
                                              target_col = "label", 
                                              target_id_col = "entity_id",
                                              stopwords = c("the"),
                                              threshold = .02,
                                              tolower = T,
                                              normalize = "none", 
                                              best_by_input_id = T, 
                                              best_by_target_id = F, 
                                              verbose = T
) |> data.table::as.data.table()
#> 
#> Fuzzy Match Summary:
#>   - Exact matches: 3/5
#>   - Fuzzy matches (good): 2/5
#>   - Deduplicated by input_id

ner_keep <- f_match_ner |> dplyr::filter(poor_match == F)

dplyr::glimpse(ner_keep)
#> Rows: 5
#> Columns: 7
#> $ input_id       <chr> "1_1_1", "2_1_1", "3_1_1", "4_1_1", "5_1_1"
#> $ input          <chr> "Republicans", "DonaldJTrump", "Hillary Clinton", "The …
#> $ matched_target <chr> "Republicans", "Donald Trump", "Hillary Clinton", "Demo…
#> $ target_id      <chr> "us-o1", "us-i1", "us-i2", "us-o2", "us-i1"
#> $ exact_match    <lgl> TRUE, FALSE, TRUE, FALSE, TRUE
#> $ distance       <dbl> 0.00000000, 0.01111111, 0.00000000, 0.00000000, 0.00000…
#> $ poor_match     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE

# Filter NER results, merge target ID
ner_filtered <- gliner_dt[ner_keep, on = c(id = "input_id"), nomatch = 0]
```

#### Masked Translation (optional)

*(Note: Currently, this does not work due to a backend conflict of
{sentiner} and {easieRnmt} – I’ll switch {easieRnmt} to `uv` and will
fix this soon.)*

As the model used for sentiment classification seems to work better on
English data, we machine translate the text. However, we want to ensure
that the named entities are preseverd as-is, that means they should not
be translated. To achieve this, we mask them and replace the placeholder
after translation.

Now we use the context column instead of the tokenized sentence only, as
the translation algorithm and sentiment classifier can handle longer
inputs.

``` r
# Install easieRnmt
# pak::pak(thieled/easieRnmt)

tl_res <- sentiner::masked_ent_translate(ner_filtered, 
                                         text_col = "text_clean", 
                                         entity_col = "entity_name", 
                                         id_col = "id" 
                                         )  

dplyr::glimpse(tl_res)
```

#### Targeted Sentiment Classification

Finally, we can predict the targeted sentiment:

``` r

sen_res <- sentiner::get_targeted_sentiment(data = ner_filtered, 
                                            text_col = "text_clean", 
                                            entity_col = "entity_name", 
                                            model = "dthiele/deberta-v3-base-targsenti-v5")
#> ℹ Classifying targeted sentiment. Please hang on...
#> Sentiment batches:   0%|          | 0/1 [00:00<?, ?batch/s]Sentiment batches: 100%|##########| 1/1 [00:02<00:00,  2.01s/batch]Sentiment batches: 100%|##########| 1/1 [00:02<00:00,  2.01s/batch]
#> ✔ Targeted sentiment classification completed in 6.9s

dplyr::glimpse(sen_res)
#> Rows: 5
#> Columns: 24
#> $ id                   <chr> "1_1_1", "2_1_1", "3_1_1", "4_1_1", "5_1_1"
#> $ sen_id               <chr> "1_1", "2_1", "3_1", "4_1", "5_1"
#> $ doc_idx              <int> 1, 2, 3, 4, 5
#> $ sen_idx              <int> 1, 1, 1, 1, 1
#> $ ent_idx              <int> 1, 1, 1, 1, 1
#> $ doc_id_u             <int> 1, 2, 3, 4, 5
#> $ text_clean           <chr> "Honestly, the Republicans have no real plan.", "…
#> $ entity_name          <chr> "Republicans", "DonaldJTrump", "Hillary Clinton",…
#> $ label                <chr> "political_party", "politician", "politician", "p…
#> $ score                <dbl> 0.9589791, 0.9351132, 0.9852717, 0.9478762, 0.935…
#> $ start                <int> 15, 23, 48, 1, 1
#> $ end                  <int> 26, 35, 63, 14, 6
#> $ ner_model            <chr> "urchade/gliner_multi-v2.1", "urchade/gliner_mult…
#> $ ner_detected         <dttm> 2026-06-26 23:30:56, 2026-06-26 23:30:56, 2026-06…
#> $ input                <chr> "Republicans", "DonaldJTrump", "Hillary Clinton",…
#> $ matched_target       <chr> "Republicans", "Donald Trump", "Hillary Clinton",…
#> $ target_id            <chr> "us-o1", "us-i1", "us-i2", "us-o2", "us-i1"
#> $ exact_match          <lgl> TRUE, FALSE, TRUE, FALSE, TRUE
#> $ distance             <dbl> 0.00000000, 0.01111111, 0.00000000, 0.00000000, 0…
#> $ poor_match           <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
#> $ sentiment            <chr> "negative", "positive", "positive", "negative", "…
#> $ sentiment_confidence <dbl> 0.9991, 0.9997, 0.9991, 0.9990, 0.9851
#> $ sentiment_model      <chr> "dthiele/deberta-v3-base-targsenti-v5", "dthiele/…
#> $ sentiment_datetime   <chr> "2026-06-26T23:31:03+00:00", "2026-06-26T23:31:03…
```
