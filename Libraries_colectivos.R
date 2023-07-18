# Principales ----
library(magrittr)
library(dplyr)
library(forcats)
library(stringr)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(e1071)     # skewness , kurtosis

library(formatR)        # provides a variety of functions for formatting and styling text, tables, and other output in R. 
library(gt)             # a tidy way to generate publication-quality tables.
library(DataExplorer)
#library(tidyquant)
library(dataCompareR)   # https://github.com/capitalone/dataCompareR/blob/master/README.md
library(httr)           # https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html

# Data tables ----
library(data.table)     # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
library(reshape2)
library(table.express)  # https://cran.r-project.org/web/packages/table.express/vignettes/table.express.html
library(dtplyr)         # https://dtplyr.tidyverse.org
library(DT)             # https://rstudio.github.io/DT/

# Markdown ----
library(kableExtra)     # https://bookdown.org/yihui/rmarkdown-cookbook/kableextra.html
                        # https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
library(knitr)          # https://yihui.org/knitr/
library(formattable)


# Date and time series ----

library(tsbox)          # https://www.tsbox.help
library(lubridate)      # https://lubridate.tidyverse.org. included in tidyverse
library(astsa)          # https://github.com/nickpoison/astsa/blob/master/astsa.pdf
library(timetk)         # https://business-science.github.io/timetk/
library(xts)
library(zoo)            # https://rstudio-pubs-static.s3.amazonaws.com/288218_117e183e74964557a5da4fc5902fc671.html
library(influxdbr)
library(rollRegres)
library(npreg)          # https://cran.r-project.org/web/packages/npreg/npreg.pdf
library(imputeTS)       # https://www.rdocumentation.org/packages/imputeTS/versions/3.2
library(TSstudio)       # https://rdrr.io/cran/TSstudio/
library(hms)            # https://github.com/tidyverse/hms
library(prospectr)      # https://cran.r-project.org/web/packages/prospectr/vignettes/prospectr.html
library(hrbrthemes)     # https://www.rdocumentation.org/packages/hrbrthemes/versions/0.8.0
library(skimr)
library(DescTools)
library(smoots)
library(locfit)
library(scorepeak)




# Graphics ----

library(ggplot2)
library(ggpubr)
library(patchwork)
library(ggthemes)
library(concaveman)  # needed for hull geometry
library(ggiraph)
library(DataVisualizations)
library(GGally)
library(ggforce)
library(ggpubr)
library(patchwork)
library(plotly)
library(ggThemeAssist)
library(esquisse)
library(dygraphs)
library(viridis)
library(ggside)

# Maps ----

library(ggmap)          # https://github.com/dkahle/ggmap/blob/master/README.md
library(googleway)
library(rayshader)

# Adicionales ----
# library(caret)
# library(lattice)
# library(kernlab)
# library(compare)
# library(maditr)             # https://github.com/gdemin/maditr
#                             # https://rstudio.github.io/reticulate/
# library(graphics)
# library(pramca)            # https://cran.r-project.org/web/packages/pracma/pracma.pdf (implementa funciones de matlab)
# 
# # Dudosas
# library(kimisc)
# library(TSAT)