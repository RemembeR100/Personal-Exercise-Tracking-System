# 🏃‍♂️ Personal Exercise Programming & Tracking System

This is a comprehensive desktop application developed using the **Racket** functional programming language. It is designed to combat sedentary lifestyles by tracking physical data, providing BMI analysis, and offering personalized exercise recommendations.

## 🚀 Key Features
- **BMI Calculation & Tracking:** Automatically calculates and monitors Body Mass Index.
- **Data Visualization:** High-quality charts for Weight, Age, and BMI analysis using the `plot` library.
- **Smart Suggestion Engine:** Recommends specific exercises (e.g., Weight Training for "Weak", Swimming for "Obese") based on health status.
- **Google Calendar Integration:** Automatically generates a calendar link to schedule recommended activities.
- **Local Database:** Uses **SQLite3** for persistent data management.

## 🛠 Tech Stack
- **Language:** Racket (Functional Programming)
- **GUI Framework:** `racket/gui`
- **Visualization:** `plot` library
- **Database:** `db` (SQLite3)
- **Integration:** Google Calendar API (via URL Template)

---

## 🇹🇷 Türkçe Özet
**Racket** fonksiyonel programlama dili kullanılarak geliştirilmiş, bireylerin fiziksel verilerini kayıt altına alan, VKİ analizi yapan ve kişiye özel spor aktiviteleri öneren bir masaüstü yazılımıdır.

### 🌟 Öne Çıkan Özellikler
- **Grafiksel Analiz:** Kilo, Yaş ve VKİ verilerini görselleştirerek kullanıcıya geri bildirim sağlar.
- **Akıllı Öneri Sistemi:** Kullanıcının sağlık durumuna göre (Zayıf, Normal, Obez vb.) otomatik spor önerileri sunar.
- **Google Takvim Entegrasyonu:** Önerilen spor aktivitesini tek tıkla takvime ekler.

---

## 📂 Project Structure / Proje Yapısı
- `kişiselegzersizprog.rkt`: Main application logic.
- `saglik_sistemi_v18_final.db`: Local SQLite database.
- `RAPOR.pdf`: Comprehensive project documentation and technical details.

## 🚀 How to Run
1. Install [Racket](https://racket-lang.org/).
2. Open `kişiselegzersizprog.rkt` in DrRacket.
3. Install necessary collections if prompted (e.g., `db`, `plot`).
4. Press **Run**.
