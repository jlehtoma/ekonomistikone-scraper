library(tidyverse)


# Read data ---------------------------------------------------------------

questions <- readr::read_csv("data/kysymykset.csv")
answers <- readr::read_csv("data/vastaukset.csv")


# Check who answered what -------------------------------------------------

answers_wide <- answers %>% 
  dplyr::select(Vastaaja, numero, Vastaus_no) %>% 
  tidyr::spread(numero, Vastaus_no)

# Answers similarity ------------------------------------------------------

answers <- answers %>%
  dplyr::mutate(Vastaus_no = Vastaus)
# Ignore warning produced, introducing NAs for "Ei vastannut" exactly what 
# needed
answers$Vastaus_no <- dplyr::recode(answers$Vastaus_no,
                                    "Vahvasti samaa mieltä" = 2,
                                    "Samaa mieltä" = 1, 
                                    "Epävarma" = 0, 
                                    "Eri mieltä" = -1, 
                                    "Vahvasti eri mieltä" = -2)

# Count mean answer for each question
mean_answer <- answers %>% 
  dplyr::group_by(numero) %>% 
  dplyr::summarise(
    ka_vastaus = mean(Vastaus_no, na.rm = TRUE)
  )

# Join the mean with the rest of the data
answers <- answers %>% 
  dplyr::left_join(mean_answer) %>% 
  # Substract the mean from each repondents answer
  dplyr::mutate(resp_diff = abs(Vastaus_no - ka_vastaus))

# Sum answer differnence by respondent
agg_resp_diff <- answers %>% 
  dplyr::group_by(Vastaaja) %>% 
  dplyr::summarise(
    diff = sum(resp_diff)
  )

# Plot data ---------------------------------------------------------------

p1 <- ggplot(answers, aes(Vastaus_no)) +
  geom_histogram(position = "dodge", binwidth = 1) + facet_wrap(~numero) + 
  theme_minimal()
