
# create params -----------------------------------
methodgroup_list <- c("Oral Contraceptives",
                      "Emergency Oral Contraceptives")

# choose the countries of interest  -----------------------------------
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

country_params <- rep(country_list, c(2,2,2,2,2,2,2,2,2,2,2,2,2)) 
                  # repeats each element twice
methodgroup_params <- rep(methodgroup_list,13)

#country_params <- "Nigeria"
#methodgroup_params <- "Oral Contraceptives"

params <- tidyr::tibble(country_params, methodgroup_params) 


# iterate params on OC_template -----------------------------------

purrr::walk2(params$country_params, 
             params$methodgroup_params, 
             ~quarto::quarto_render(
                              input = "OC_template.qmd",
                              execute_params = list(country = .x,
                                                    methodgroup = .y),
                              output_file = glue::glue("{.y}_{.x}.pptx")
                            ))


  