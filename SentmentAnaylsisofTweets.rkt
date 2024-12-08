#lang racket
(require net/url)
(require racket/string)  ; For string manipulation functions
(require plot)
(require data-science)

; Function to remove punctuation and special symbols from a string
(define (remove-punctuation-and-symbols str)
  (define punctuations "[[:punct:]]")  ; Regular expression to match punctuation characters
  (define special-symbols "[^a-zA-Z0-9 ]")  ; Remove any non-alphanumeric characters (including special symbols)
  (define without-punctuation (regexp-replace* punctuations str "")) ; Remove punctuation
  (regexp-replace* special-symbols without-punctuation "")) ; Remove special symbols

; Function to read CSV data from a string
(define (csv-string->list csv-str)
  (map (lambda (line) 
         (map (lambda (field) (string-trim field)) (string-split line ",")))
       (string-split csv-str "\n")))

; Function to fetch, clean, and return specific column data from a URL
(define (fetch-clean-and-extract-column url column-index)
  ; Convert the URL string to a URL object
  (define url-object (string->url url))

  ; Open a connection to the URL
  (define in (get-pure-port url-object #:redirections 5))

  ; Read the content from the URL
  (define csv-content (port->string in))

  ; Close the input port
  (close-input-port in)

  ; Convert CSV string into a list of lists (rows)
  (define csv-data (csv-string->list csv-content))

  ; Function to extract a specific column by index
  (define (extract-column data column-index)
    (map (lambda (row) 
           (if (< column-index (length row)) ; Ensure index is within range
               (list-ref row column-index)
               (error "Column index out of range"))) 
         data))

  ; Extract the column data
  (define column-data (extract-column csv-data column-index))

  ; Clean the column data (remove punctuation, special symbols, convert to lowercase, normalize spaces)
  (map (lambda (value)
         (if (string? value)
             (string-normalize-spaces
              (remove-punctuation-and-symbols
               (string-downcase value)))  ; Perform cleaning on a string
             value))  ; Leave non-string values unchanged
       column-data))

; Function to tokenize and sort words from cleaned column data
(define (tokenize-and-sort-column column-index url)
  (define column-data (fetch-clean-and-extract-column url column-index))
  (define document-text (string-join column-data " ")) ; Combine all rows into a single "document"
  (document->tokens document-text #:sort? #t)) ; Tokenize and sort words

; Function to analyze sentiment using NRC lexicon
(define (analyze-sentiment column-index url)
  (define tokenized-words (tokenize-and-sort-column column-index url)) ; Tokenize and sort words
  (list->sentiment tokenized-words #:lexicon 'nrc)) ; Perform sentiment analysis using NRC lexicon

; Function to calculate the total frequency of each sentiment
(define (count-sentiments sentiment-data)
  ; Group by sentiment type
  (define grouped-sentiments (group-by (lambda (entry) (first entry)) sentiment-data))

  ; Sum frequencies for each sentiment and return as list of pairs
  (map (lambda (group)
         (let ([sentiment (first (first group))] ; Extract the sentiment
               [total-frequency (apply + (map second group))]) ; Sum the frequencies
           (list sentiment total-frequency))) ; Return (sentiment, frequency)
       grouped-sentiments))

; Function to plot the sentiment frequencies
(define (plot-sentiments counts)
  (parameterize ((plot-width 800))
    (plot (list
           (tick-grid)
           (discrete-histogram
            (sort counts (λ (x y) (> (second x) (second y)))) ; Sort by frequency
            #:color "MediumSlateBlue"
            #:line-color "MediumSlateBlue"))
          #:x-label "Sentiment"
          #:y-label "Frequency")))

; Procedure to fetch, process and plot sentiment data
(define (process-sentiment-data url column-index)
  ; Step 1: Perform sentiment analysis
  (define sentiment (analyze-sentiment column-index url))

  ; Step 2: Extract sentiment and frequency data (excluding the header)
  (define sentiment-data (map (lambda (entry) (list (second entry) (third entry))) (rest sentiment)))

  ; Step 3: Count the occurrences of each sentiment
  (define sentiment-counts (count-sentiments sentiment-data))

  ; Step 4: Display sentiment counts (optional)
  (for-each (lambda (entry) (displayln entry)) sentiment-counts)

  ; Step 5: Plot the sentiment counts
  (plot-sentiments sentiment-counts))

; Example usage: Fetch and process data from the provided URL and column index
(process-sentiment-data "https://raw.githubusercontent.com/smagezi95/BillingData/main/sentiment141.csv" 5)
