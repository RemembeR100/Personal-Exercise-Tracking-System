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
;; 2. ANALİZ PENCERESİ
;; =============================================================================

(define (analiz-ekranini-ac)
  (define f (new frame% [label "Detaylı Analiz Merkezi"] [width 1300] [height 850]))
  (define main-panel (new vertical-panel% [parent f] [border 15]))
  (define secili-grafik 0) 

  (define tabs 
    (new tab-panel% 
         [parent main-panel] 
         [choices '("Kilo & VKİ Analizi" "Hasta Dağılımı" "Yaş & VKİ Analizi")]
         [font (make-object font% 12 'default 'normal 'bold)] 
         [callback (lambda (tp e)
                     (set! secili-grafik (send tp get-selection))
                     (send ana-tuval refresh))]))

  (define grafik-paneli (new vertical-panel% [parent tabs] [border 10]))
  
  (define ana-tuval 
    (new canvas% 
         [parent grafik-paneli]
         [paint-callback 
          (lambda (c dc)
            (define-values (w h) (send c get-client-size))
            (with-handlers ([exn:fail? (lambda (ex) (bos-veri-mesaji dc w h "GRAFİK ÇİZİLEMEDİ! VERİ YETERSİZ."))])
              (cond 
                ;; --- GRAFİK 1: KİLO & VKİ ---
                [(= secili-grafik 0)
                 (define rows (query-rows baglanti "SELECT kilo, vki FROM hasta_havuzu"))
                 (if (empty? rows) (bos-veri-mesaji dc w h "VERİ YOK! LÜTFEN KİŞİ EKLEYİNİZ.")
                     (let ()
                       (define kilolar (map (lambda (r) (vector-ref r 0)) rows))
                       (define vkiler (map (lambda (r) (vector-ref r 1)) rows))
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
                 (if (empty? rows) (bos-veri-mesaji dc w h "VERİ YOK! LÜTFEN KİŞİ EKLEYİNİZ.")
                     (let ()
                       (define counts (make-hash))
                       (for ([r rows]) (define d (vector-ref r 0)) (hash-set! counts d (+ 1 (hash-ref counts d 0))))
                       (define plot-data (for/list ([(k v) (in-hash counts)]) (vector k v)))
                       (plot/dc (discrete-histogram plot-data #:color 4 #:label "Kişi Sayısı")
                                dc 0 0 w h 
                                #:title "Durum Dağılımı" #:x-label "Kategoriler" #:y-label "Kişi Sayısı")))]
                
                ;; --- GRAFİK 3: YAŞ ANALİZİ ---
                [(= secili-grafik 2)
                 (define rows (query-rows baglanti "SELECT yas, vki FROM hasta_havuzu"))
                 (if (empty? rows) (bos-veri-mesaji dc w h "VERİ YOK! LÜTFEN KİŞİ EKLEYİNİZ.")
                     (let ()
                       (define yaslar (map (lambda (r) (vector-ref r 0)) rows))
                       (define vkiler (map (lambda (r) (vector-ref r 1)) rows))
                       (define min-yas (- (apply min yaslar) 10)) 
                       (define max-yas (+ (apply max yaslar) 10))
                       (define min-vki (- (apply min vkiler) 5))
                       (define max-vki (+ (apply max vkiler) 5))
                       (define pts (map vector yaslar vkiler))
                       
                       (plot/dc (points pts #:color 'red #:sym 'fullsquare #:size 14 #:label "Yaş")
                                dc 0 0 w h 
                                #:title "Yaş ve VKİ Dağılımı" #:x-label "Yaş" #:y-label "VKİ"
                                #:x-min min-yas #:x-max max-yas
                                #:y-min min-vki #:y-max max-vki)))])))]))
                                
  (define (bos-veri-mesaji dc w h mesaj)
    (send dc set-scale 1.5 1.5)
    (send dc set-text-foreground "red")
    (send dc draw-text mesaj (- (/ w 3) 150) (- (/ h 3) 50)))

  (send f show #t))

;; =============================================================================
;; 3. ANA EKRAN VE FONTLAR
;; =============================================================================

(define title-font (make-object font% 16 'default 'normal 'bold))
(define section-font (make-object font% 12 'default 'normal 'bold))
(define normal-font (make-object font% 10 'default 'normal 'normal))

(define frame (new frame% [label "Profesyonel Sağlık Sistemi - Akıllı Arayüz"] [width 1300] [height 800]))
(define main-split (new horizontal-panel% [parent frame] [border 15] [spacing 25]))

;; --- SOL PANEL ---
(define left-panel (new vertical-panel% [parent main-split] [stretchable-width #f] [min-width 400] [spacing 15]))

;; 3.1 İstatistik Paneli
(define g-stat (new group-box-panel% [parent left-panel] [label "📊 Sistem Özeti"] [font section-font]))
(define lbl-toplam-hasta (new message% [parent g-stat] [label "Toplam Kayıt: 0"] [font normal-font] [auto-resize #t]))
(define lbl-ort-vki (new message% [parent g-stat] [label "Ortalama VKİ: 0.0"] [font normal-font] [auto-resize #t]))

;; 3.2 Veri Giriş Paneli
(define g-giris (new group-box-panel% [parent left-panel] [label "✏️ Veri Girişi"] [font section-font]))
(define txt-ad (new text-field% [parent g-giris] [label "Ad Soyad:"] [font normal-font]))
(define txt-yas (new text-field% [parent g-giris] [label "Yaş:"] [font normal-font]))
(define txt-boy (new text-field% [parent g-giris] [label "Boy (cm):"] [font normal-font]))
(define txt-kilo (new text-field% [parent g-giris] [label "Kilo (kg):"] [font normal-font]))

(define lbl-sonuc (new message% [parent left-panel] 
                       [label "Durum: -" ] [color "blue"] 
                       [font (make-object font% 13 'default 'normal 'bold)]
                       [auto-resize #t] [min-width 380]))

(define h-btn-panel (new horizontal-panel% [parent left-panel] [alignment '(center center)] [spacing 15]))

(define btn-hesapla (new button% [parent h-btn-panel] 
                         [label "🔍 Sadece Hesapla"] [font normal-font] [min-height 40]
                         [callback (lambda (b e) (sadece-hesapla))]))

(define btn-ekle (new button% [parent h-btn-panel] 
                      [label "💾 Datasete Ekle"] [font normal-font] [min-height 40]
                      [callback (lambda (b e) (dataset-ekle))]))

;; 3.3 Hızlı İşlemler Paneli
(define g-islemler (new group-box-panel% [parent left-panel] [label "⚙️ Hızlı İşlemler"] [font section-font]))
(new button% [parent g-islemler] [label "📅 SEÇİLİ KİŞİYİ TAKVİME EKLE"] 
     [min-height 45] [font normal-font]
     [callback (lambda (b e) (takvime-ekle-tikla))])

(new button% [parent g-islemler] [label "🗑️ SEÇİLİ KİŞİYİ SİL"] 
     [min-height 40] [font normal-font]
     [callback (lambda (b e) (kisi-sil))])

(new button% [parent left-panel] [label "📈 DETAYLI ANALİZ MERKEZİ (TAM SAYFA)"] 
     [min-height 55] [font (make-object font% 11 'default 'normal 'bold)]
     [callback (lambda (b e) (analiz-ekranini-ac))])

;; --- SAĞ PANEL ---
(define right-panel (new vertical-panel% [parent main-split] [spacing 15]))

;; 3.4 Arama Paneli
(define g-arama (new horizontal-panel% [parent right-panel] [stretchable-height #f] [spacing 10]))
(define txt-arama (new text-field% [parent g-arama] [label "🔍 Hasta Ara (Ad Soyad):"] [font normal-font]
                       [callback (lambda (t e) (dataset-listele (send t get-value)))]))
(new button% [parent g-arama] [label "✖ Temizle"] [font normal-font]
     [callback (lambda (b e) (send txt-arama set-value "") (dataset-listele ""))])

;; 3.5 Liste
(define g-liste (new group-box-panel% [parent right-panel] [label "📋 Kayıtlı Hasta Listesi"] [font section-font]))
(define list-dataset (new list-box% 
                          [parent g-liste] 
                          [label #f] 
                          [choices '()] 
                          [style '(single column-headers)] 
                          [columns '("Ad Soyad" "Yaş" "Boy" "Kilo" "VKİ" "Durum")]
                          [font (make-object font% 10 'default)]
                          [min-height 500]))

(send list-dataset set-column-width 0 220 50 400)
(send list-dataset set-column-width 1 60 20 100)
(send list-dataset set-column-width 2 70 20 100)
(send list-dataset set-column-width 3 70 20 100)
(send list-dataset set-column-width 4 70 20 100)
(send list-dataset set-column-width 5 200 50 400)

;; =============================================================================
;; 4. FONKSİYONLAR
;; =============================================================================

(define (istatistik-guncelle)
  (with-handlers ([exn:fail? (lambda (ex) (void))])
    (define total (query-value baglanti "SELECT COUNT(*) FROM hasta_havuzu"))
    (define avg-vki (query-value baglanti "SELECT AVG(vki) FROM hasta_havuzu"))
    (send lbl-toplam-hasta set-label (format "Toplam Kayıt: ~a" total))
    (send lbl-ort-vki set-label (format "Ortalama VKİ: ~a" (if (sql-null? avg-vki) "0.0" (~r avg-vki #:precision 1))))))

(define (dataset-listele [arama-metni ""])
  (send list-dataset clear)
  (define q (if (string=? (string-trim arama-metni) "")
                "SELECT id, ad_soyad, yas, boy, kilo, vki, durum FROM hasta_havuzu ORDER BY id DESC"
                (format "SELECT id, ad_soyad, yas, boy, kilo, vki, durum FROM hasta_havuzu WHERE ad_soyad LIKE '%~a%' ORDER BY id DESC" (string-replace arama-metni "'" "''"))))
  
  (with-handlers ([exn:fail? (lambda (ex) (message-box "Hata" "Veriler listelenirken hata oluştu." frame))])
    (define rows (query-rows baglanti q))
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
  (istatistik-guncelle))

(define (girdi-dogrula ad yas-str boy-str kilo-str)
  (cond
    [(or (string=? (string-trim ad) "") (string=? (string-trim yas-str) "") 
         (string=? (string-trim boy-str) "") (string=? (string-trim kilo-str) ""))
     "Lütfen tüm alanları doldurun."]
    [(not (string->number yas-str)) "Yaş alanı sadece sayı olmalıdır."]
    [(not (string->number boy-str)) "Boy alanı sadece sayı olmalıdır."]
    [(not (string->number kilo-str)) "Kilo alanı sadece sayı olmalıdır."]
    [(<= (string->number boy-str) 0) "Boy sıfırdan büyük olmalıdır."]
    [(<= (string->number kilo-str) 0) "Kilo sıfırdan büyük olmalıdır."]
    [(<= (string->number yas-str) 0) "Yaş sıfırdan büyük olmalıdır."]
    [else #t]))

(define (sadece-hesapla)
  (define ad (send txt-ad get-value))
  (define yas-str (send txt-yas get-value))
  (define boy-str (send txt-boy get-value))
  (define kilo-str (send txt-kilo get-value))
  
  (define dogrulama (girdi-dogrula ad yas-str boy-str kilo-str))
  (if (eq? dogrulama #t)
      (let* ([boy (string->number boy-str)] [kilo (string->number kilo-str)]
             [boy-m (/ boy 100.0)] [vki (/ kilo (* boy-m boy-m))]
             [durum (cond [(< vki 18.5) "Zayıf"] [(< vki 25) "Normal"] [(< vki 30) "Fazla Kilolu"] [(< vki 35) "Obezite Tip 1"] [else "Obezite Tip 2+"])])
        (send lbl-sonuc set-label (format "SONUÇ: ~a (VKİ: ~a)" durum (~r vki #:precision 1))))
      (message-box "Giriş Hatası" dogrulama frame)))

(define (dataset-ekle)
  (define ad (send txt-ad get-value))
  (define yas-str (send txt-yas get-value))
  (define boy-str (send txt-boy get-value))
  (define kilo-str (send txt-kilo get-value))
  
  (define dogrulama (girdi-dogrula ad yas-str boy-str kilo-str))
  (if (eq? dogrulama #t)
      (let* ([yas (string->number yas-str)] [boy (string->number boy-str)] [kilo (string->number kilo-str)]
             [boy-m (/ boy 100.0)] [vki (/ kilo (* boy-m boy-m))]
             [durum (cond [(< vki 18.5) "Zayıf"] [(< vki 25) "Normal"] [(< vki 30) "Fazla Kilolu"] [(< vki 35) "Obezite Tip 1"] [else "Obezite Tip 2+"])])
        
        (with-handlers ([exn:fail? (lambda (ex) (message-box "Veritabanı Hatası" "Kişi eklenirken bir hata oluştu." frame))])
          (query-exec baglanti "INSERT INTO hasta_havuzu (ad_soyad, yas, boy, kilo, vki, durum, tarih) VALUES (?, ?, ?, ?, ?, ?, date('now'))" ad yas boy kilo vki durum)
          (send lbl-sonuc set-label (format "KAYDEDİLDİ: ~a (VKİ: ~a)" durum (~r vki #:precision 1)))
          (send txt-ad set-value "") (send txt-yas set-value "") (send txt-boy set-value "") (send txt-kilo set-value "")
          (dataset-listele)
          (message-box "Başarılı" "Kişi başarıyla eklendi." frame)))
      (message-box "Giriş Hatası" dogrulama frame)))

(define (kisi-sil)
  (define secilen-indeks (send list-dataset get-selection))
  (if secilen-indeks
      (let* ([db-id (send list-dataset get-data secilen-indeks)]
             [ad (send list-dataset get-string secilen-indeks)]
             [cevap (message-box "Silme Onayı" (format "'~a' isimli hastayı silmek istediğinize emin misiniz?" ad) frame '(yes-no))])
        (when (eq? cevap 'yes)
          (with-handlers ([exn:fail? (lambda (ex) (message-box "Hata" "Kişi silinemedi." frame))])
            (query-exec baglanti "DELETE FROM hasta_havuzu WHERE id = ?" db-id)
            (dataset-listele)
            (message-box "Başarılı" "Kişi silindi." frame))))
      (message-box "Uyarı" "Lütfen silmek için listeden bir kişi seçiniz." frame)))

(define (takvime-ekle-tikla)
  (define secilen-indeks (send list-dataset get-selection))
  (if secilen-indeks
      (let* ([db-id (send list-dataset get-data secilen-indeks)])
        (with-handlers ([exn:fail? (lambda (ex) (message-box "Hata" "Veritabanı bağlantı hatası." frame))])
          (define row (query-maybe-row baglanti "SELECT ad_soyad, durum FROM hasta_havuzu WHERE id = ?" db-id))
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
              (message-box "Hata" "Veri bulunamadı." frame))))
      (message-box "Uyarı" "Lütfen listeden bir kişi seçiniz." frame)))

;; Uygulamayı Başlat
(dataset-listele)
(send frame show #t)