
# Introduction ------------------------------------------------------------

# Purpose: Present the story of OC trends in a country using multiple data sources
# Author: Arati Krishnamoorthy
# Version Control:
# Nov 2023  Build off the GSC.r script
# new learning:
#   use slice to remove rows
#   use janitor row_to_names to get column names from a row
#   use janitor remove_empty to remove rows that have all NAs
#   use relocate
#   use fill
#   negating the condition for a filter
# use RcolorBrewer for ggplots
# setting the theme for a ggplot
# use forcats functions to set/change factor levels
# casting a variable as a factor in order to change order on a stacked bar plot
# using rep

# old learning:
#   use across to perform operations across multiple columns
#   use \(x) instead of ~.
#   use if_all and a function/condition with filter
#   change case_when default to .default =
# use count(is.na(variable)) to count number of nas
#
# Initial Setup -----------------------------------------------------------

# load packages
pacman::p_load("tidyverse",
               "rio",      # for import/export
               "janitor",  # for data cleaning
               "here",     # for relative file paths
         #      "ggthemes", # for a colorblind safe color palette
               "ggplot2",
               "RColorBrewer"
            )


# For exploring character or factor columns, it is useful to have a see_distinct function
see_distinct <- function(df, var){
  df |> 
    distinct({{ var }}) |> 
    arrange({{ var }}) |> 
    pull()
}


# Setting the theme for visualization 

color_title <- "#202020" #  "#000000"
color_caption <- "#909090"  # "#000000"
color_plot_text <- color_plot_text <- "#505050" # "#000000"
      
line <- 10
    
mytheme <- function() {
      theme_classic() +
        theme(plot.title = element_text(size = 14,
                                        color = color_title,
                                        face = "bold",
                                        margin = margin(b = line),
                                        hjust = 0),
              
              plot.title.position = "plot", #Move plot.title to the left
              
              plot.subtitle = element_text(size = 8,
                                           face = "italic",
                                           color = color_plot_text,
                                           hjust = 0), 
              
              plot.caption = element_text(size = 8,
                                          color = color_caption,
                                          face = "italic",
                                          margin = margin(t = line),
                                          hjust = 0, # left-align caption
                                          vjust = 1),
              
              plot.caption.position = "plot",
              
              plot.margin = ggplot2::margin(15, 15, 10, 15), #top, right, bottom, left
              
              axis.title = element_text(size = 10, # adjusts both axes titles
                                        color = color_plot_text),          
              axis.text.x = element_text(size = 8,
                                         angle = 60, 
                                         hjust = 1,
                                         color = color_plot_text),
              axis.text.y = element_text(size = 8,
                                         color = color_plot_text),
              
              axis.ticks = element_blank(),
              
              # axis.line = element_blank(),
              
              #              legend.position = "none"              
              legend.position = "right",
              legend.title = element_blank(),
              legend.text = element_text(size = 8,
                                         color = color_plot_text)
              
              # text =  element_text(color = color_plot_text,
              # this will change all text size (except geom_text)
              #                          size = 8,
              #                          hjust = 0.5)
        )
      
    }


# DKT : Import data -------------------------------------------------------------

# Import DKT data 
dkt_raw <- import(here("raw_data", "Historical-CSM-Statistics-1991-2021-v2023.06.12.xlsx"),
                  na = c("", " ","NA"),
                  )

dkt_importdate <- "Nov 2023"

#TO DO check why using import results in "" not being read as NAs

# DKT : Data exploration, wrangling ---------------------------------------------


dkt_raw |> glimpse()
dkt_raw |> head(n=15)

# clean names
# can remove the first 11 rows
dkt <- dkt_raw |> 
  janitor::clean_names() |> 
  slice(-(1:11)) |>  # same as filter(row_number() > 11)
  glimpse()

# column names are in row 2 (years)
dkt <- dkt |> 
  janitor::row_to_names(row_number = 2, remove_rows_above = FALSE) |> 
  janitor::clean_names() |>
  glimpse()

# this first column contains mostly the method information so needs to be renamed
dkt <- dkt |> 
  rename(method = started_2003) 

# delete all the rows that are completely empty
dkt <- dkt |> 
  janitor::remove_empty(which = "rows")

# remove the rows that have just the years in them as this information is now in the header
# in these cases the x1991 column = 1991
# also delete rows that have method= "Total CYPs" or "TOTAL CYPs" in them (244)

dkt <- dkt |> 
  mutate(method = str_squish(method),
         x1991 = str_squish(x1991),
         program = str_squish(program))

dkt |> #243
  filter(x1991 == "1991") |> 
  select(method, x1991)

dkt <- dkt |> # should be 2301-243=2058 but it is 2054??
  filter(!str_starts(method, "Started|started|no|Year"))

dkt |> #244
  filter(str_detect(method, "CYPs")) 

dkt <- dkt |> # should be 2054-244=1810 
  filter(!str_detect(method, "CYPs"))

# check why the year columns came in as char
dkt |> 
  distinct(x1995)
# make all the year columns numeric
dkt <- dkt |> 
   mutate(across(x1991:x2022, \(x) as.numeric(x)))

# create a country column 
# find the rows where all year cols are NA (238), these have the country program names
dkt %>% 
  filter(if_all(c(x1991:x2022), ~ is.na(.x))) |> 
  distinct(method)

# if all year cols are NA, then country program name is in the method column
dkt <- dkt %>%  
  mutate(countryprogram = if_else(if_all(c(x1991:x2022), ~ is.na(.x)),
                                  method,
                                  NA)
         ) |> 
# move the newly created columns to the front
  relocate(countryprogram, .after = method) |> 
# fill the countryprogram column in the down direction
  fill(countryprogram, .direction = "down")  |> 
# remove the header rows for the countryprogram where all year columns are NA
  filter(!if_all(c(x1991:x2022), ~ is.na(.x))) 

dkt |> 
  distinct(countryprogram) |> 
  arrange(countryprogram)

# separate country program 
dkt <- dkt |> 
  separate_wider_delim(cols = countryprogram,
                       delim = "(",
                       names = c("country","program"),
                       too_many = "merge",
                       cols_remove = FALSE
                       ) |> 
  mutate(program = str_remove_all(program,regex("\\)|\\("))) |> 
  mutate(country = str_squish(country),
         program = str_squish(program))

dkt |> 
  distinct(country, program) |> 
  arrange(country, program) |> 
  print(n=300)

# Fix some errors in the country names
# India, LLC is India
# Nigeria I is Nigeria

dkt <- dkt |> 
  mutate(country = case_when(
                    str_detect(country, regex("India", ignore_case = TRUE)) ~ "INDIA",
                    str_detect(country,regex("Nigeria",ignore_case = TRUE)) ~ "NIGERIA",
                    .default = country),
         country = str_to_title(country)
         )

dkt |> 
  distinct(country, program) |> 
  arrange(country, program) |> 
  print(n=300)

# clean up program names
dkt <- dkt |> 
  mutate(program = case_when(
    program == "AMOS/USAID" ~ "ASMO/USAID",
    program == "SOCPIETY FOR FAMILY HEALTH" ~ "SOCIETY FOR FAMILY HEALTH",
    .default = program)
  )


# Clean up the method
dkt |> 
  see_distinct(method)

dkt <- dkt |> 
  mutate(
    method = str_remove_all(method, regex("\\.")),
    method = case_when(str_detect(method, regex("cycle beads", ignore_case = TRUE))
                       ~ "Cycle Beads",
                       
                       str_detect(method, regex("diaphragm",ignore_case = TRUE))
                       ~ "Diaphragms",
                       
                       method == "Female condoms"
                       ~ "Female Condoms",
                       
                       (method == "Foam Tabs")|
                         (method == "Foaming Tablets")|
                         (method == "VFT")|
                         (method == "Vaginal Foaming Tabs")
                       ~ "Vaginal Foaming Tablets",
                      
                       str_detect(method, regex("Implant", ignore_case = TRUE)) 
                       ~ "Implants",
                      
                       str_detect(method, regex("Injectable",ignore_case = TRUE)) 
                      ~ "Injectables",
                      
                      method == "EC"
                      ~ "Emergency Oral Contraceptives",
                      
                      (method == "OCP")
                      ~ "Oral Contraceptives",
                      
                      method == "Free OCP"
                      ~ "Free Oral Contraceptives",
                      
                      str_detect(method, regex("Tabs",ignore_case = TRUE)) 
                      ~ "Tablets",
                      
                      method == "Mifepristone"
                      ~ "Mifepristone Tablets",
                      
                      method == "Misoprostol"
                      ~ "Misoprostol Tablets",
                      
                      str_detect(method, regex("MVA",ignore_case=TRUE))
                      ~ "MVA Kits",
                      
                      method == "MA ‘Combipack’ (Mifepristone Tablets-Misoprostol)‘Combipack’ (Mifepristone Tablets-Misoprostol)"
                      ~ "MA ‘Combipack’ (Mifepristone Tablets-Misoprostol)",
                      method == "MA ‘Combipack’ (Mifepristone Tablets-Misoprostol)at 50%"
                      ~ "MA ‘Combipack’ (Mifepristone Tablets-Misoprostol) at 50%",
                    
                      .default = method
                      )
    )

dkt |> 
  see_distinct(method)

dkt <- dkt |> 
  mutate(method_group = factor(
    case_when(str_detect(method, "Condoms")                       
              ~ "Condoms",
              method %in% c("Oral Contraceptives",
                            "Free Oral Contraceptives")
              ~ "Oral Contraceptives",
             
              (str_detect(method, "Tablets|Medabon|MPAC")) & (method != "Vaginal Foaming Tablets") & (method != "Tablets")
              ~ "Abortion Pills",
              str_detect(method, "MVA")                       
              ~ "MVA Kits",
              str_detect(method, "IUD")
              ~ "IUDs",
              method == "Norplant"
              ~ "Implants",
              method == "Noristerat"
              ~ "Injectables",
           
              .default = method))) 

dkt |> 
  see_distinct(method_group)

dkt |> 
  distinct(method_group,method) |> 
  arrange(method_group, method) |> 
  print(n=2000)

# pivot longer the years
dkt <- dkt |> 
          pivot_longer(cols = c(x1991:x2022),
               names_to = "year",
               values_to = "qty_delivered",
               values_drop_na = TRUE)

dkt <- dkt |> 
  mutate(year = str_remove(year,"x"),
          year = factor(year))

dkt |> 
  see_distinct(year)

# ignore this code to calculate CYP 
#TO DO add CYP for all methods
dkt <- dkt |> 
  mutate(CYP_conversion = case_when(method=="Emergency Oral Contraceptives"
                                    ~ (1/14),
                                    method=="Free Oral Contraceptives"
                                    ~ (1/16),
                                    method=="Oral Contraceptives"
                                    ~ (1/16),
                                    .default = NA
                                    )
           )
dkt <- dkt |> 
  mutate(CYP_delivered = qty_delivered*CYP_conversion)

dkt |> 
  distinct(method_group,method, CYP_conversion) |> 
  arrange(method_group, method, CYP_conversion) |> 
  print(n=2000)

# DKT : Data visualization ---------------------------------------------

# countries of interest
country_list <- c("Nigeria",
                  "Afghanistan",
                  "Benin",
                  "Burkina Faso",
                  "Ghana",
                  "Madagascar",
                  "Mali",
                  "Mozambique",
                  "Senegal",
                  "Tanzania",
                  "Togo",
                  "Uganda",
                  "Zambia")

dkt |> see_distinct(method_group)
methodgroup_list <- c("Oral Contraceptives",
                      "Emergency Oral Contraceptives") # not including tablets

country_params <- rep(country_list,2)
methodgroup_params <- rep(methodgroup_list,13)

generate_dktcharts <- function(choose_country,choose_methodgroup){

  dktchart <-
    ggplot(dkt |> 
           filter(  (country == choose_country) 
                  & (method_group == choose_methodgroup) 
                  ),
         aes(x = year, y = qty_delivered, fill = program)) +
    
    geom_bar(position = "stack", stat = "identity") +
    
    scale_fill_brewer(palette = "YlGnBu") +
  
    labs(
      title = str_c("SMO sales",
                    "of", choose_methodgroup, "in",
                    choose_country, 
                    sep = " "),
      caption = str_c("Data Source: DKT International Contraceptive Social Marketing Statistics\n (https://www.dktinternational.org/contraceptive-social-marketing-statistics/)\nData last updated:",
                      dkt_importdate,
                      "\nMethods included:",
                      choose_methodgroup,
                     # "\nDKT uses the following conversion, which accounts for loss and wastage, to calculate CYP: OCs 1 CYP = 16 cycles; ECs 1 CYP = 14 doses",
                      sep = " "),
      x = "year sold",
      y = "Quantity sold in cycles"
    ) +
  
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    mytheme()

    ggsave(filename = str_c(choose_country,"_",choose_methodgroup,".png"),
           device = "png",
           plot = last_plot(),
           path = "C:\\Users\\akrishnamoorthy\\Documents\\Github\\OC\\plots\\DKT")

    return(list(dktchart))
    
}

purrr::map2(country_params, methodgroup_params, generate_dktcharts) 

# VAN : Import data -------------------------------------------------------------

# raw file has shipments for select countries for oral contraceptives
van_raw <- import(here("raw_data", "shipment.xlsx"),
                  na = c("", " ","NA"),
                  skip = 1
                  )

van_importdate <- "Nov 2023"

#TO DO check why using import results in "" not being read as NAs

# VAN : Data exploration, wrangling ---------------------------------------------

van_raw |> glimpse()
van_raw |> head(n=1)

# clean names
# can remove the first 11 rows
van <- van_raw |> 
  janitor::clean_names() |> 
  glimpse()

# select columns of interest
van <- van |> 
  select(country_name,
         supplier_description,
         shipment_status,
         shipment_line_status,
         estimated_ship_date,
         estimated_delivery_date,
         shipment_mode,
         ship_to_party_name,
         funder,
         procurer,
         program_recipient_description,
         product_description,
         shipped_qty,
         base_uom,
         manufacturer_description,
         l3_method,
         l5_product,
         source_uom,
         uom_value_factor,
         uom_qty_factor,
         estimated_unit_value_usd,
         data_provider,
         data_visibility,
         customer_type,
         program_recipient_id,
         delivered_date,
         received_date,
         delivered_qty,
         received_qty
         ) |> 
  glimpse()

van |> 
  distinct(l3_method, source_uom, uom_qty_factor, base_uom) 

van |> 
  mutate(across(.cols = where(is.POSIXct), .fns = as_date)) |> 
  glimpse()

# create a field that shows funder-recipient

van |> see_distinct(funder)

van <- van |> 
  mutate(funder = case_when((is.na(funder))|(funder=="TBD") 
                            ~ "Unknown",
                            .default = funder))
             
van |> see_distinct(program_recipient_description)
van |> see_distinct(program_recipient_id)
van <- van |>
  mutate(program_recipient_description = 
           str_replace(string = program_recipient_description,
           pattern = "unspecified",
           replacement = "Unspecified"))

van <- van |>
  mutate(program_recipient_description = 
           str_replace(string = program_recipient_description,
                       pattern = "Organisation",
                       replacement = "Organization"))

van <- van |> 
  mutate(program_recipient_description =
           case_when((str_detect(program_recipient_description,
                                "Unspecified non-governmental organization"))|
                       (str_detect(program_recipient_description,
                                   "Program Unspecified"))
                            ~ "Program Unspecified",
                            .default = program_recipient_description))

van <- van |>
  mutate(program_recipient_description = 
           str_replace(string = program_recipient_description,
                       pattern = "MoH",
                       replacement = "MOH"))


van <- van |>
  mutate(program_recipient_description = 
           str_replace(string = program_recipient_description,
                       pattern = "T-MARC",
                       replacement = "TMARC"))

van <- van |> 
  unite("funder_recipient", 
        funder, program_recipient_description, 
        remove=FALSE)

van |> see_distinct(funder_recipient)
van |> count(funder_recipient, sort = TRUE) |> pull(funder_recipient)

# convert funder_recipietn to factor and set levels per frequency
van <- van |> 
  mutate(funder_recipient = factor(funder_recipient,
                                   levels = c(
                                     "Unknown_NA",
                                     "Other_Ministry of Health (MOH)",
                                     "Other_Other United Nations Agency",
                                     "Other_Program Unspecified",
                                     "DKT_DKT",
                                     "DKT_NA",
                                     "Medical Export Group B.V. (MEG)_Medical Export Group B.V. (MEG)",
                                     "MSI_MSI Reproductive Choices",
                                     "Global Fund_Other United Nations Agency",
                                     "Global Fund_Program Unspecified",
                                     "Global Fund_Ministry of Health (MOH)",
                                     "IPPF_Benin Assoc. for Promotion of the Family (ABPF) [IPPF]",
                                     "IPPF_Burkina Association for Family Welfare (ABBEF) [IPPF]",
                                     "IPPF_Fianakaviana Sambatra (FISA) [IPPF]",
                                     "IPPF_Malian Association for Family Planning (AMPPF) [IPPF]",
                                     "IPPF_Planned Parenthood Association of Ghana (PPAG) [IPPF]",
                                     "IPPF_Planned Parenthood Association of Zambia (PPAZ) [IPPF]",
                                     "IPPF_Planned Parenthood Federation of Nigeria (PPFN) [IPPF]",
                                     "IPPF_Reproductive Health Uganda (RHU) [IPPF]",
                                     "IPPF_Togolese Association for Family Welfare (ATBEF) [IPPF]",
                                     "WAHO_Benin Association for Social Marketing (ABMS) [PSI]",
                                     "WAHO_NA",
                                     "WAHO_Program for Social Mktg. and Health Comm. (PROMACO)",
                                     "WAHO_Program Unspecified",
                                     "WAHO_Ministry of Health (MOH)",
                                     "UNFPA C.O. Co-Financing Agreement_Program Unspecified",
                                     "UNFPA Supplies_Program Unspecified",
                                     "USAID_Afghan Social Marketing Organization (ASMO)",
                                     "USAID_Agency for the Dev. of Social Mktg. (ADEMAS) [PSI]",
                                     "USAID_Benin Assoc. for Promotion of the Family (ABPF) [IPPF]",
                                     "USAID_Benin Association for Social Marketing (ABMS) [PSI]",
                                     "USAID_Burkina Association for Family Welfare (ABBEF) [IPPF]",
                                     "USAID_FHI360",
                                     "USAID_Health Keepers Network (HKN)",
                                     "USAID_Helping Mothers and Children Thrive (HEMAYAT) [Jhpiego]",
                                     "USAID_Joint Medical Store (JMS)",
                                     "USAID_Kampala Pharm",
                                     "USAID_Keneya Jemu Kan (KJK) [Johns Hopkins CCP]",
                                     "USAID_Marie Stopes Burkina Faso [MSI]",
                                     "USAID_Momentum [PSI]",
                                     "USAID_Momentum [PSI]/Jigi",
                                     "USAID_Office of U.S. Foreign Disaster Assistance (OFDA)",
                                     "USAID_Palladium",
                                     "USAID_Population Services International (PSI)",
                                     "USAID_Program for Social Mktg. and Health Comm. (PROMACO)",
                                     "USAID_Social Marketing Company (SMC)",
                                     "USAID_Society for Family Health (SFH) [PSI]",
                                     "USAID_Tanzania Marketing and Communication (TMARC)",
                                     "USAID_Togolese Association for Social Marketing (ATMS)",
                                     "USAID_Total Family Health Organization (TFHO)",
                                     "USAID_Uganda Health Marketing Group (UHMG)",
                                     "USAID_USAID DISCOVER Health Project [JSI]",
                                     "USAID_Program Unspecified",
                                     "USAID_Ministry of Health (MOH)"
                                     
                                   )))
                  
         #funder_recipient = forcats::fct_infreq(funder_recipient), #sorts levels by frequency, with largest first
         #funder_recipient = forcats::fct_rev(funder_recipient) #inverts the order of levels so most freq appear at the bottom of bar charts
   

levels(van$funder_recipient) 


# check status
van |>  see_distinct(shipment_status)

# check and fill nas in delivered_date
van |> count(is.na(delivered_date)) 

van <- van |>
  mutate(delivered_date_calc = coalesce(delivered_date, received_date)) 

van |> count(is.na(delivered_date_calc)) 

van <- van |>
  mutate(delivered_date_calc = coalesce(delivered_date, estimated_delivery_date)) 

van |> count(is.na(delivered_date_calc)) 

van <- van |> 
  mutate(delivered_year = as_factor(year(delivered_date_calc)))

van |> see_distinct(delivered_year)         

# check for nas in delivered_qty
van |> count(is.na(delivered_qty)) 

van <- van |>
  mutate(delivered_qty_calc = coalesce(delivered_qty, received_qty)) 
van |> count(is.na(delivered_qty_calc)) 

van <- van |>
  mutate(delivered_qty_calc = coalesce(delivered_qty_calc, shipped_qty)) 
van |> count(is.na(delivered_qty_calc)) 

# create method groups so oral contraceptives is one group

van <- van |> 
  mutate(method_group = case_when((l3_method == "Combined Oral Contraceptives"|
                                  l3_method == "Progestogen Only Pills")
                                  ~ "Oral Contraceptives",
                                  .default = l3_method))
                                  
    
van |> glimpse()

# VAN : Data visualization ---------------------------------------------

van |> distinct(method_group)

methodgroup_list <- c("Oral Contraceptives",
                      "Emergency Oral Contraceptives")
country_params <- rep(country_list,2)
methodgroup_params <- rep(methodgroup_list,13)

van |> 
  see_distinct(funder_recipient)

generate_vancharts <- function(choose_country, choose_methodgroup){
  
  vanchart <- 
    ggplot(van |> 
         filter(country_name == choose_country &
                method_group == choose_methodgroup
                ),
  #           mutate(cyp = case_when(l3_method == choose_method1
  #                                 ~ delivered_qty/15,
  #                                 l3_method == choose_method2
  #                                 ~ delivered_qty/12,
  #                                 l3_method == choose_method3
  #                                 ~ delivered_qty/20)
  #                 ),
       aes(x = delivered_year, 
           y = delivered_qty_calc,   # Note: delivered quantity in base_uom
           #y = cyp,
           fill = funder_recipient)) + 
  
  geom_bar(position = "stack", stat = "identity") +
  
  scale_fill_brewer(palette = "YlGnBu") +
    
  labs(
    title = str_c("Shipment of",
                  choose_methodgroup,
                  "to", choose_country, 
                  sep = " "),
    subtitle = ("Colors represent different Funder_Recipient combinations"),
    caption = str_c("Data source: Global Family Planning Visibility and Analytics Network (GFPVAN) \nNote that GFPVAN data is governed by the GFPVAN terms of use and is not for distribution external to USAID \nData last updated:",
                    van_importdate,
                    #"\nUnit of measure: ",
                    # method_uom,
               #    "\nData includes",
               #   choose_methodgroup,
                   # "\nThe following conversion was used to calculate CYP: COCs 1 CYP = 15 cycles; Progestin only pills 1 CYP = 12 cycles; ECs 1 CYP = 20 doses",
                    sep = " "),
    x = "year of delivery",
    y = "quantity delivered in cycles"
   # y = "CYP delivered"
  ) +
  
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  mytheme()
  
  ggsave(filename = str_c(choose_country,"_",choose_methodgroup,".png"),
         device = "png",
         plot = last_plot(),
         path = "C:\\Users\\akrishnamoorthy\\Documents\\Github\\OC\\plots\\GFPVAN")
  

  return(vanchart)
  
}

purrr::map2(country_params, methodgroup_params, generate_vancharts) 

# TODO 

# show USAID_MOH in the same color across charts and lowest bar
# quarto with parameters to create country slides! -- copy VAN code and then do quarto params
