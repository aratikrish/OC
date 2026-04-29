**Contraceptive Distribution Trends: Multi-Source Analysis**

Parameterized Quarto report analyzing oral contraceptive distribution trends across countries, 
built for USAID's Office of Population and Reproductive Health.

**Overview**

This project combines data from two sources to visualize contraceptive shipment and sales trends by country:  
  - GFPVAN (Global Family Planning Visibility and Analytics Network): Shipment data showing quantities delivered by funder and recipient organization  
  - DKT International: Social marketing organization (SMO) sales data for contraceptive products across countries   
 
The analysis produces side-by-side visualizations comparing shipment volumes and SMO sales over time, disaggregated by funder-recipient combination and program.  
The quarto report template generates slides for any country and method group combination, enabling scalable, repeatable analysis across multiple countries  

**Repository Structure**  
OC/  
├── OC_template.qmd    # Parameterized Quarto report template  
├── scripts/           # Supporting R scripts  
├── raw_data/          # Source data (not included; governed by data use agreements)  
└── OC.Rproj           # RStudio project file   


