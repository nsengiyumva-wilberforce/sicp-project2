;NSENGIYUMVA WILBERFORCE
;2024/HD05/22078U
;2400722078
#lang racket

(require data-science-master)
(require plot)
(require math)
(require json)

;;; Function to read line-oriented JSON (output from massmine)
;;; This function converts the JSON lines into an array
;;; It is suitable for small data sets, but not ideal for large datasets
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()] ; Initialize the array to store JSON records
             [record (read-json (current-input-port))])
    (if (or (eof-object? record) ; End of file or reached specified head limit
            (and head (>= num head)))
        (jsexpr->string json-array) ; Return JSON array as string
        (loop (add1 num)             ; Otherwise, continue reading the file
              (cons record json-array)
              (read-json (current-input-port))))))

;;; Read the entire tweet database from "country_tweets.json"
;;; Using the json-lines->json-array function to convert the JSON lines
(define tweets (string->jsexpr
                (with-input-from-file "country_tweets.json" 
                  (λ () (json-lines->json-array)))))


;;; Process and clean the tweet data
;;; Extract tweet texts, partition data, and created dates from the tweets
(define t
  (let ([tmp (map (λ (x) (list (hash-ref x 'text) 
                               (hash-ref x 'partition_1)
                               (hash-ref x 'created_at))) 
                  (car tweets))])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))  ; Exclude retweets

;;; Classify each tweet based on its originating country (Egypt, Kuwait, or Uganda, or Other)
(define tweet-by-country
  (map (λ (x) (list (first x)
                    (cond [(string-contains? (second x) "Egypt") "egypt"]
                          [(string-contains? (second x) "Kuwait") "kuwait"]
                          [(string-contains? (second x) "Uganda") "uganda"]
                          [else "other"])) )
       t))

;;; Separate the tweets by country
(define egypt (filter (λ (x) (string=? (second x) "egypt")) tweet-by-country))
(define kuwait (filter (λ (x) (string=? (second x) "kuwait")) tweet-by-country))
(define uganda (filter (λ (x) (string=? (second x) "uganda")) tweet-by-country))

;;; Text preprocessing function for tweets
;;; It removes punctuation, URLs, and converts text to lowercase
(define (process-text str)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase str)) #:websafe? #t)))

;;; Tokenize and remove stopwords
(define (tokenize-and-remove-stopwords tweets)
  (map (λ (x)
         (remove-stopwords
          (document->tokens (process-text x) #:sort? #t)))
       tweets))

;;; Tokenize and remove stopwords from Egyptian tweets
(define e (tokenize-and-remove-stopwords ($ egypt 0)))

;;; Tokenize and remove stopwords from Kuwaiti tweets
(define k (tokenize-and-remove-stopwords ($ kuwait 0)))

;;; Tokenize and remove stopwords from Uganda tweets
(define u (tokenize-and-remove-stopwords ($ uganda 0)))

;;; Analyze sentiment using the NRC lexicon
(define (analyze-sentiment tokens)
  (list->sentiment tokens #:lexicon 'nrc))

;;; Analyze sentiment for each country
(define e-sentiment (analyze-sentiment (append* e)))
(define k-sentiment (analyze-sentiment (append* k)))
(define u-sentiment (analyze-sentiment (append* u)))

;;; Aggregate sentiment counts (frequency of each sentiment label)
(define (aggregate-sentiment-counts sentiment-data)
  (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq)))

;;; Aggregate sentiment counts for each country
(define e-sentiment-counts (aggregate-sentiment-counts e-sentiment))
(define k-sentiment-counts (aggregate-sentiment-counts k-sentiment))
(define u-sentiment-counts (aggregate-sentiment-counts u-sentiment))

;;; Plot sentiment analysis
(define (plot-sentiment-analysis sentiment-counts country)
  (parameterize ((plot-width 800)) 
    (plot (list
           (tick-grid)
           (discrete-histogram
            (sort sentiment-counts (λ (x y) (> (second x) (second y)))) 
            #:color "red"
            #:line-color "MediumSlateBlue"))
          #:x-label "Affective Label"
          #:y-label "Frequency"
          #:title (string-append "Frequency of Moods for " country " Tweets"))))

;;; User Input for country
(define user-input (read-line)) ; Get user input

;;; Validate input and display sentiment plot
(cond [(string=? user-input "Egypt") 
       (plot-sentiment-analysis e-sentiment-counts "Egypt")]
      [(string=? user-input "Kuwait") 
       (plot-sentiment-analysis k-sentiment-counts "Kuwait")]
      [(string=? user-input "Uganda") 
       (plot-sentiment-analysis u-sentiment-counts "Uganda")]
      [else (display "Invalid input. Please enter either 'Egypt' or 'Kuwait' or 'Uganda.")])
