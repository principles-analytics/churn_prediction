# will be better to use renv in a future iteration

suppressPackageStartupMessages(library(tidymodels)) 
suppressPackageStartupMessages(library(readr))       
suppressPackageStartupMessages(library(yardstick))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(pins))
suppressPackageStartupMessages(library(plumber))
suppressPackageStartupMessages(library(rapidoc))
suppressPackageStartupMessages(library(vetiver))
suppressPackageStartupMessages(library(DALEXtra))
suppressPackageStartupMessages(library(DALEX))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(randomNames))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(markdown))

source(here::here("utils-pins.R"))
source(here::here("utils-exploration.R"))
source(here::here("utils-modeling.R"))
source(here::here("utils-db.R"))
source(here::here("utils-data-generation.R"))

# can help some ML engines
cores <- parallel::detectCores()

# some globals (making it simple to read but should be in env file)
board <- pins::board_folder("./model_versions")
model_name <- "xgb_model"
model_name_vetiver <- "xgb_model_vetiver"
fpath_data <- here::here("data", "churn_data.csv")

