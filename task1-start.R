# prepare data
df_enea = read.csv('data/ENEA.mst')
names(df_enea) = c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
df_enea$date = as.Date.character(df_enea$date, format = '%Y%m%d')
df_enea$year = format(df_enea$date, format = '%Y')
df_enea_2019 = subset(df_enea, df_enea$year == 2019)

df_kghm = read.csv('data/KGHM.mst')
names(df_kghm) = c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
df_kghm$date = as.Date.character(df_kghm$date, format = '%Y%m%d')
df_kghm$year = format(df_kghm$date, format = '%Y')
df_kghm_2019 = subset(df_kghm, df_kghm$year == 2019)


# task 1a
df_enea_2019$open_ch = with(df_enea_2019, c(NA, 100*diff(open)/open[1:length(open) -1]))
plot(open_ch ~ date, df_enea_2019,
     type = 'l', col = 'blue',
     xlab = 'Data otwarcia spółki', ylab = 'Zmiana ceny otwarcia [%]',
     main = 'Procentowe zmiany cen otwarcia spółki ENEA w roku 2019')
grid()

df_kghm_2019$open_ch = with(df_kghm_2019, c(NA, 100*diff(open)/open[1:length(open) -1]))
plot(open_ch ~ date, df_kghm_2019,
     type = 'l', col = 'blue',
     xlab = 'Data otwarcia spółki', ylab = 'Zmiana ceny otwarcia [%]',
     main = 'Procentowe zmiany cen otwarcia spółki KGHM w roku 2019')
grid()


# task 1b
hist(df_enea_2019$open_ch, breaks = 50, prob = T,
     xlab = 'Zmiana kursu otwarcia [%] ',
     ylab = 'Częstość występowania',
     main = 'Histogram procentowych zmian kursu spółki ENEA w roku 2019')
grid()

boxplot(df_enea_2019 $open_ch, col = 'green',
        xlab = 'ENEA',
        ylab = 'Zmiana kursu otwarcia [%] ',
        main = 'ENEA' )
grid()

hist(df_kghm_2019$open_ch, breaks = 50, prob = T,
     xlab = 'Zmiana kursu otwarcia [%] ',
     ylab = 'Częstość występowania',
     main = 'Histogram procentowych zmian kursu spółki KGHM w roku 2019')
grid()

boxplot(df_kghm_2019 $open_ch, col = 'green',
        xlab = 'KGHM',
        ylab = 'Zmiana kursu otwarcia [%] ',
        main = 'KGHM' )
grid()


# task 1c
m_enea = mean(df_enea_2019$open_ch, na.rm = T)
s_enea = sd(df_enea_2019$open_ch, na.rm = T)

m_kghm = mean(df_kghm_2019$open_ch, na.rm = T)
s_kghm = sd(df_kghm_2019$open_ch, na.rm = T)


# task 1d
hist(df_enea_2019$open_ch, breaks = 50, prob = T,
     xlab = 'Zmiana kursu otwarcia [%] ',
     ylab = 'Częstość występowania',
     main = 'Histogram oraz rozkład normalny procentowych zmian kursu spółki ENEA w roku 2019')
curve(dnorm(x, mean = m_enea, sd = s_enea), add = T, col = 'red', -10, 10)
grid()


hist(df_kghm_2019$open_ch, breaks = 50, prob = T,
     xlab = 'Zmiana kursu otwarcia [%] ',
     ylab = 'Częstość występowania',
     main = 'Histogram oraz rozkład normalny procentowych zmian kursu spółki KGHM w roku 2019')
curve(dnorm(x, mean = m_kghm, sd = s_kghm), add = T, col = 'red', -10, 10)
grid()
