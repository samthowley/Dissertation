
# Single source of truth for paper -> approximate lat/long, shared by site_map.R
# and the meta-analysis MLM scripts (03_Scripts/Streams/analysis/metaanalysis_mlm_*.R).
# Extracted out of site_map.R so both consumers stay in sync automatically --
# edit here, not in either downstream script.

# APPROXIMATE coordinates, catchment/study-area level (not exact sampling
# points). Sourced from each paper's Location description (Paper_Info sheet
# of meta_analysis_v3.xlsx) -- keep this in sync with the Citation set in the
# Data sheet, since a paper missing here drops out with no lat/long match.
site_coords <- tibble::tribble(
  ~Citation,                       ~lat,    ~lon,
  "(Aho et al., 2021)",             41.90,  -72.90,  # Connecticut River Watershed, NW CT
  "(Bega et al., 2026)",           -22.00,  -47.90,  # Broa/Espraiado/Canchim etc., São Carlos, São Paulo, Brazil
  "(Bernal et al., 2022)",          41.75,    2.50,  # La Tordera catchment, Catalonia, Spain
  "(Bertuzzo et al., 2022)",        37.69,  -92.68,  # Centroid of 5 CONUS reaches (TN/WI/UT/TX/WV)
  "(Carter et al., 2022)",          35.97,  -79.05,  # New Hope Creek, Duke Forest, NC
  "(Crawford et al., 2014)",        46.03,  -89.66,  # Trout Lake catchment, N. Highlands Lake District, WI
  "(Demars, 2019)",                 57.92,   -2.55,  # Glensaugh research station, Aberdeenshire, Scotland, UK
  "(Duvert et al., 2019)",         -13.50,  131.30,  # Daly/Howard River, Northern Territory, AUS
  "(Gong et al., 2021)",            31.30,  119.40,  # Tianmu Lake catchment, Zhejiang/Jiangsu, China
  "(Gómez-Gener et al., 2016)",     42.15,    2.75,  # Fluvia River network, Catalonia, NE Spain
  "(Hall et al., 2026)",            48.15, -114.10,  # Blaine Creek, Creston, Flathead Valley, MT
  "(Khadka et al., 2014)",          29.85,  -82.60,  # Santa Fe River watershed, north-central FL
  "(Kirk & Cohen, 2023)",           29.90,  -82.50,  # Santa Fe River network, north-central FL
  "(Leng et al., 2025)",            52.00,   11.50,  # Bode @ Gross Germersleben / Elbe @ Magdeburg, Germany
  "(Liu et al., 2026)",             36.30,  113.95,  # Midpoint of South-to-North Water Diversion canal, China
  "(Lupon et al., 2019)",           64.21,   19.77,  # Krycklan catchment, near Umea, Sweden
  "(Marzolf et al., 2022)",         10.43,  -83.99,  # La Selva Biological Station, Costa Rica
  "(Moustapha et al., 2022)",        3.50,   11.50,  # Nyong watershed, Cameroon
  "(Nguyen et al., 2025)",          47.60,    2.60,  # Loire River at Dampierre, France
  "(Oviedo-Vargas et al., 2015)",   10.43,  -83.99,  # La Selva Biological Station, Costa Rica
  "(Piatka et al., 2024)",          47.70,   11.50,  # Haselbach, ~50 km S of Munich, Bavaria, Germany
  "(Rasilo et al., 2017)",          50.45,  -66.70,  # Côte-Nord boreal streams, Québec, Canada
  "(Rexroade et al., 2026)",       -13.13,  130.79,  # Litchfield National Park, Northern Territory, AUS
  "(Rocher-Ros et al., 2020)",      68.35,   18.82,  # Miellajokka catchment, near Abisko, Sweden
  "(Shangguan et al., 2026)",       47.05, -109.60,  # Louse/Porter Creeks, Judith River Watershed, central MT
  "(Solano et al., 2023)",         -12.87,  131.12,  # Manton Creek, near Darwin, NT, AUS
  "(Taillardat et al., 2022)",      50.52,  -63.20,  # La Romaine watershed, Quebec, Canada
  "(Wang et al., 2021)",            38.28,  109.73,  # Hailiutu River, Yulin City, Shaanxi, China
  "(Wang et al., 2023)",            38.35,  109.65   # Bulang River, Hailiutu catchment, Ordos Basin, China
)
