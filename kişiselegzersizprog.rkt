#lang racket/gui
(require net/sendurl)
(require db)
(require racket/string)
(require plot)
(require net/uri-codec)

;; =============================================================================
;; 1. VERİTABANI BAĞLANTISI
;; =============================================================================

(define baglanti 
  (sqlite3-connect #:database "saglik_sistemi_v18_final.db" #:mode 'create))

(query-exec baglanti 
            "CREATE TABLE IF NOT EXISTS hasta_havuzu (
               id INTEGER PRIMARY KEY,
               ad_soyad TEXT,
               yas INTEGER,
               boy INTEGER,
               kilo REAL,
               vki REAL,
               durum TEXT,
               tarih TEXT)")

;; =============================================================================
;; 2. ANALİZ PENCERESİ (DÜZELTİLMİŞ YAŞ GRAFİĞİ)
;; =============================================================================

(define (analiz-ekranini-ac)
  (define f (new frame% [label "Detaylı Analiz Merkezi"] [width 1300] [height 850]))
  (define main-panel (new vertical-panel% [parent f]))
  (define secili-grafik 0) 

  (define tabs 
    (new tab-panel% 
         [parent main-panel] 
         [choices '("Kilo & VKİ Analizi" "Hasta Dağılımı" "Yaş & VKİ Analizi")]
         [font (make-object font% 11 'default)] 
         [callback (lambda (tp e)
                     (set! secili-grafik (send tp get-selection))
                     (send ana-tuval refresh))]))

  (define grafik-paneli (new vertical-panel% [parent tabs]))
  
  (define ana-tuval 
    (new canvas% 
         [parent grafik-paneli]
         [paint-callback 
          (lambda (c dc)
            (define-values (w h) (send c get-client-size))
            (cond 
              ;; --- GRAFİK 1: KİLO & VKİ ---
              [(= secili-grafik 0)
               (define rows (query-rows baglanti "SELECT kilo, vki FROM hasta_havuzu"))
               (if (empty? rows) (bos-veri-mesaji dc w h)
                   (let ()
                     (define kilolar (map (lambda (r) (vector-ref r 0)) rows))
                     (define vkiler (map (lambda (r) (vector-ref r 1)) rows))
                     ;; Geniş Boşluk Bırakıyoruz
                     (define min-kilo (- (apply min kilolar) 15))
                     (define max-kilo (+ (apply max kilolar) 15))
                     (define min-vki (- (apply min vkiler) 10))
                     (define max-vki (+ (apply max vkiler) 10))
                     (define pts (map vector kilolar vkiler))
                     
                     (plot/dc (points pts #:color 'blue #:sym 'fullcircle #:size 14 #:label "Hasta")
                              dc 0 0 w h 
                              #:title "Kilo ve VKİ İlişkisi" #:x-label "Kilo" #:y-label "VKİ"
                              #:x-min min-kilo #:x-max max-kilo 
                              #:y-min min-vki #:y-max max-vki)))]
              
              ;; --- GRAFİK 2: DAĞILIM ---
              [(= secili-grafik 1)
               (define rows (query-rows baglanti "SELECT durum FROM hasta_havuzu"))
               (if (empty? rows) (bos-veri-mesaji dc w h)
                   (let ()
                     (define counts (make-hash))
                     (for ([r rows]) (define d (vector-ref r 0)) (hash-set! counts d (+ 1 (hash-ref counts d 0))))
                     (define plot-data (for/list ([(k v) (in-hash counts)]) (vector k v)))
                     (plot/dc (discrete-histogram plot-data #:color 4 #:label "Kişi")
                              dc 0 0 w h 
                              #:title "Durum Dağılımı" #:x-label "Kategoriler" #:y-label "Sayı")))]
              
              ;; --- GRAFİK 3: YAŞ ANALİZİ (DÜZELTİLDİ: ARTIK ORTALIYOR) ---
              [(= secili-grafik 2)
               (define rows (query-rows baglanti "SELECT yas, vki FROM hasta_havuzu"))
               (if (empty? rows) (bos-veri-mesaji dc w h)
                   (let ()
                     (define yaslar (map (lambda (r) (vector-ref r 0)) rows))
                     (define vkiler (map (lambda (r) (vector-ref r 1)) rows))
                     
                     ;; Sol üstte sıkışmaması için geniş aralık veriyoruz
                     (define min-yas (- (apply min yaslar) 10)) 
                     (define max-yas (+ (apply max yaslar) 10))
                     (define min-vki (- (apply min vkiler) 5))
                     (define max-vki (+ (apply max vkiler) 5))

                     (define pts (map vector yaslar vkiler))
                     
                     (plot/dc (points pts #:color 'red #:sym 'fullsquare #:size 14 #:label "Yaş")
                              dc 0 0 w h 
                              #:title "Yaş ve VKİ" #:x-label "Yaş" #:y-label "VKİ"
                              #:x-min min-yas #:x-max max-yas
                              #:y-min min-vki #:y-max max-vki)))]))]))

  (define (bos-veri-mesaji dc w h)
    (send dc set-scale 1.5 1.5)
    (send dc set-text-foreground "red")
    (send dc draw-text "VERİ YOK! LÜTFEN KİŞİ EKLEYİNİZ." (- (/ w 3) 150) (- (/ h 3) 50)))

  (send f show #t))

;; =============================================================================
;; 3. ANA EKRAN
;; =============================================================================

(define frame (new frame% [label "Profesyonel Sağlık Sistemi"] [width 1250] [height 750]))
(define main-split (new horizontal-panel% [parent frame] [border 10] [spacing 15]))

(define left-panel (new vertical-panel% [parent main-split] [stretchable-width #f] [min-width 350]))

(define g-giris (new group-box-panel% [parent left-panel] [label "Veri Girişi"] [font (make-object font% 10 'default)]))
(define txt-ad (new text-field% [parent g-giris] [label "Ad Soyad:"]))
(define txt-yas (new text-field% [parent g-giris] [label "Yaş:"]))
(define txt-boy (new text-field% [parent g-giris] [label "Boy (cm):"]))
(define txt-kilo (new text-field% [parent g-giris] [label "Kilo (kg):"]))

(define lbl-sonuc (new message% [parent left-panel] 
                       [label "Durum: -" ] [color "blue"] 
                       [font (make-object font% 11 'default 'normal 'bold)]
                       [auto-resize #t] [min-width 350]))

(define h-btn-panel (new horizontal-panel% [parent left-panel] [alignment '(center center)] [spacing 10]))

(define btn-hesapla (new button% [parent h-btn-panel] 
                         [label "Sadece Hesapla"] [min-height 35]
                         [callback (lambda (b e) (sadece-hesapla))]))

(define btn-ekle (new button% [parent h-btn-panel] 
                      [label "Datasete Ekle"] [min-height 35]
                      [callback (lambda (b e) (dataset-ekle))]))

(new button% [parent left-panel] [label "SEÇİLİ KİŞİYİ TAKVİME EKLE"] 
     [min-height 50] [font (make-object font% 10 'default 'normal 'bold)]
     [callback (lambda (b e) (takvime-ekle-tikla))])

(new button% [parent left-panel] [label "ANALİZ MERKEZİ (TAM SAYFA)"] 
     [min-height 45] [font (make-object font% 10 'default 'normal 'bold)]
     [callback (lambda (b e) (analiz-ekranini-ac))])

(define right-panel (new group-box-panel% [parent main-split] [label "Kayıtlı Hasta Listesi"]))

(define list-dataset (new list-box% 
                          [parent right-panel] 
                          [label #f] 
                          [choices '()] 
                          [style '(single column-headers)] 
                          [columns '("Ad Soyad" "Yaş" "Boy" "Kilo" "VKİ" "Durum")]
                          [font (make-object font% 9 'default)]
                          [min-height 500]))

(send list-dataset set-column-width 0 200 50 300)
(send list-dataset set-column-width 1 50 20 100)
(send list-dataset set-column-width 2 60 20 100)
(send list-dataset set-column-width 3 60 20 100)
(send list-dataset set-column-width 4 60 20 100)
(send list-dataset set-column-width 5 150 50 300)

;; =============================================================================
;; 4. FONKSİYONLAR
;; =============================================================================

(define (dataset-listele)
  (send list-dataset clear)
  (define rows (query-rows baglanti "SELECT id, ad_soyad, yas, boy, kilo, vki, durum FROM hasta_havuzu ORDER BY id DESC"))
  
  (for ([r rows])
    (define id (vector-ref r 0)) 
    (define ad (vector-ref r 1))
    (define yas (number->string (vector-ref r 2)))
    (define boy (number->string (vector-ref r 3)))
    (define kilo (~r (vector-ref r 4) #:precision 1))
    (define vki (~r (vector-ref r 5) #:precision 1))
    (define durum (vector-ref r 6))
    
    (send list-dataset append ad)
    (define n (- (send list-dataset get-number) 1))
    
    (send list-dataset set-data n id) ;; ID GİZLE
    
    (send list-dataset set-string n yas 1) 
    (send list-dataset set-string n boy 2)
    (send list-dataset set-string n kilo 3) 
    (send list-dataset set-string n vki 4)
    (send list-dataset set-string n durum 5)))

(define (sadece-hesapla)
  (define boy (string->number (send txt-boy get-value)))
  (define kilo (string->number (send txt-kilo get-value)))
  (if (and boy kilo (> boy 0))
      (let* ([boy-m (/ boy 100.0)] [vki (/ kilo (* boy-m boy-m))]
             [durum (cond [(< vki 18.5) "Zayıf"] [(< vki 25) "Normal"] [(< vki 30) "Fazla Kilolu"] [(< vki 35) "Obezite Tip 1"] [else "Obezite Tip 2+"])])
        (send lbl-sonuc set-label (format "SONUÇ: ~a (VKİ: ~a)" durum (~r vki #:precision 1))))
      (message-box "Hata" "Boy ve Kilo giriniz." frame)))

(define (dataset-ekle)
  (define ad (send txt-ad get-value)) (define yas (string->number (send txt-yas get-value)))
  (define boy (string->number (send txt-boy get-value))) (define kilo (string->number (send txt-kilo get-value)))
  (if (and ad yas boy kilo (> boy 0))
      (let* ([boy-m (/ boy 100.0)] [vki (/ kilo (* boy-m boy-m))]
             [durum (cond [(< vki 18.5) "Zayıf"] [(< vki 25) "Normal"] [(< vki 30) "Fazla Kilolu"] [(< vki 35) "Obezite Tip 1"] [else "Obezite Tip 2+"])])
        (send lbl-sonuc set-label (format "KAYDEDİLDİ: ~a (VKİ: ~a)" durum (~r vki #:precision 1)))
        (query-exec baglanti "INSERT INTO hasta_havuzu (ad_soyad, yas, boy, kilo, vki, durum, tarih) VALUES (?, ?, ?, ?, ?, ?, date('now'))" ad yas boy kilo vki durum)
        (dataset-listele)
        (send txt-ad set-value "") (send txt-yas set-value "") (send txt-boy set-value "") (send txt-kilo set-value "")
        (message-box "Başarılı" "Kişi eklendi." frame))
      (message-box "Hata" "Bilgileri giriniz." frame)))

(define (takvime-ekle-tikla)
  (define secilen-indeks (send list-dataset get-selection))
  (if secilen-indeks
      (let* ([db-id (send list-dataset get-data secilen-indeks)])
        (define row 
          (if (number? db-id)
              (query-maybe-row baglanti "SELECT ad_soyad, durum FROM hasta_havuzu WHERE id = ?" db-id)
              (let ([ad (send list-dataset get-string secilen-indeks)])
                (query-maybe-row baglanti "SELECT ad_soyad, durum FROM hasta_havuzu WHERE ad_soyad = ? ORDER BY id DESC LIMIT 1" ad))))
        
        (if row
            (let* ([ad (vector-ref row 0)]
                   [durum (vector-ref row 1)]
                   [aktivite (cond 
                               [(string-contains? durum "Zayıf") "Kas Kutlesi Artirma (Agirlik)"]
                               [(string-contains? durum "Normal") "Form Koruma (Kosu ve Fitness)"]
                               [(string-contains? durum "Fazla Kilolu") "Yag Yakimi (Tempolu Yuruyus)"]
                               [(string-contains? durum "Obezite Tip 1") "Eklem Dostu (Yuzme ve Bisiklet)"]
                               [(string-contains? durum "Obezite Tip 2") "Kontrollu Fizik Tedavi"]
                               [else "Genel Saglik Egzersizi"])]
                   [mesaj (format "Hasta: ~a\nDurum: ~a\n\nSistem Önerisi: ~a\n\nGoogle Takvim açılsın mı?" ad durum aktivite)])
              
              (define cevap (message-box "Takvim Onayı" mesaj frame '(yes-no)))
              
              (when (eq? cevap 'yes)
                (define link 
                  (string-append "https://calendar.google.com/calendar/render?action=TEMPLATE&text=" 
                                 (uri-encode (string-append "Spor: " aktivite)) 
                                 "&details=" (uri-encode (string-append "Hasta: " ad " - Durum: " durum))))
                (send-url link)))
            (message-box "Hata" "Veri bulunamadı." frame)))
      (message-box "Uyarı" "Listeden bir kişi seçiniz." frame)))

(dataset-listele)
(send frame show #t)