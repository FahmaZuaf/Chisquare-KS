# kelompok10

**`kelompok10`** adalah package R yang menyediakan:

- Simulasi data dari berbagai **distribusi probabilitas diskrit dan kontinu**
- Uji **Chi-Square** untuk data diskrit
- Uji **Kolmogorov–Smirnov (KS)** untuk data kontinu

Package ini dikembangkan untuk keperluan pembelajaran Komputasi Statistika.

---

## ✨ Instalasi

```
# Install devtools jika belum
install.packages("devtools")

# Install package
devtools::install_github("FahmaZuaf/Chisquare-KS")
```

## 🔧 Fitur Utama

1. **Simulasi Data: `simulate_data()`**

Fungsi untuk menghasilkan data acak dari berbagai distribusi probabilitas, baik diskrit maupun kontinu.

### Distribusi yang didukung:

- **Diskrit**:
  - `"bernoulli"`: parameter `prob`
  - `"binomial"`: parameter `size`, `prob`
  - `"poisson"`: parameter `lambda`
  - `"geometric"`: parameter `prob`
- **Kontinu**:
  - `"normal"`: parameter `mean`, `sd`
  - `"exponential"`: parameter `rate`
  - `"gamma"`: parameter `shape`, `rate`
  - `"beta"`: parameter `shape1`, `shape2`

### Contoh:

```r
simulate_data(100, "binomial", list(2, 0.5))
simulate_data(100, "normal", list(mean = 0, sd = 1))
```

2. **Uji Chi-Square: `chi_square_test()`**

Fungsi untuk melakukan uji Chi-Square (Goodness-of-Fit) untuk data diskrit, baik dengan parameter teoretik maupun estimasi otomatis.

```r
# Simulasi data binomial

x <- simulate_data(100, "binomial", list(2, 0.5))

# Uji dengan estimasi otomatis

chi_square_test(x)

# Uji dengan distribusi dan parameter teoretik

chi_square_test(x, "binomial", list(size = 2, prob = 0.5))
```

3. **Uji Kolmogorov–Smirnov: `ks_test()`**

Fungsi untuk melakukan uji Kolmogorov–Smirnov pada data kontinu.

Distribusi yang didukung:

- `"normal"`
- `"exponential"`
- `"gamma"`
- `"beta"`

```r
# Simulasi data gamma
x <- simulate_data(100, "gamma", list(2, 1))

# Uji dengan estimasi otomatis
ks_test(x, "gamma")

# Uji dengan parameter teoretik
ks_test(x, "gamma", list(shape = 2, rate = 1))
```

## 🧪 Pengujian

```r
devtools::test()
```

## 📄 Lisensi

Lisensi: GPL-3

## 👥 Pengembang

Kelompok 10
(Sri Wahyuni, Fahma Zuaf Zarir, Arya Dirga Alfiqhri, Larasati Oktarani)

Maintainer: fahmazuafzarir14@gmail.com
