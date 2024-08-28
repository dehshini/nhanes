# load data manually

nhanes01_02 <- read_xpt("/Users/dehshini/code/R/nhanes/data/01_02/DEMO_B.XPT")

nhanes01_02 <- nhanes01_02  %>% 
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/01_02/ALQ_B.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/01_02/BMX_B.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/01_02/BPQ_B.XPT"))


nhanes03_04 <- read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/DEMO_C.XPT")

nhanes03_04 <- nhanes03_04  %>% 
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/BPX_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/BMX_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/BPQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/CDQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/DIQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/DUQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/HIQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/HSQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/HUQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L10_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L10AM_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L13_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L13AM_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L16_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L25_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/L40_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/MCQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/PAQ_C.XPT")) %>%
    left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/RXQ_RX_C.XPT"))
    # left_join(read_xpt("/Users/dehshini/code/R/nhanes/data/03_04/SMQ_C.XPT"))
