bkg_clean_matched_addresses <- function(messy_data, cols, identifiers, verbose) {
  zip_code <- ifelse(length(cols) == 4, cols[3], cols[2])
  place <- ifelse(length(cols) == 4, cols[4], cols[3])
  
  if (isTRUE(identifiers)) {
    identifiers <- c("rs", "nuts", "inspire")
  }

  if (isTRUE(verbose)) {
    cli::cli_progress_step(
      msg = "Cleaning up geocoding output...",
      msg_done = "Cleaned up geocoding output.",
      msg_failed = "Failed to clean up geocoding output."
    )
  }

  is_out <- grepl("\\.y$", names(messy_data))
  is_inp <- grepl("\\.x$", names(messy_data))
  new_out <- gsub("\\.y$", "_output", names(messy_data)[is_out])
  new_in <- gsub("\\.x$", "_input", names(messy_data)[is_inp])
  names(messy_data)[is_out] <- new_out
  names(messy_data)[is_inp] <- new_in

  # Clean dataset
  clean_data <- tibble::tibble(
    .iid = messy_data$.iid,
    score = messy_data$score,
    address_input = paste(
      messy_data$whole_address_input,
      messy_data[[paste0(zip_code, "_input")]],
      messy_data[[paste0(place, "_input")]]
    ),
    street_input = messy_data$street_input,
    house_number_input = messy_data$house_number_input,
    zip_code_input = messy_data$zip_code_input,
    place_input = messy_data$place_input,
    address_output = messy_data$whole_address_add,
    street_output = messy_data$street_output,
    house_number_output = messy_data$house_number_output,
    zip_code_output = messy_data$zip_code_output,
    place_output = messy_data$place_output,
    RS  = messy_data$RS,
    AGS = paste0(substr(messy_data$RS, 1, 5), substr(messy_data$RS, 10, 12)),
    VWG = substr(messy_data$RS, 1, 9),
    KRS = substr(messy_data$RS, 1, 5),
    RBZ = substr(messy_data$RS, 1, 3),
    STA = substr(messy_data$RS, 1, 2),
    x = messy_data$x,
    y = messy_data$y,
    source = "\u00a9 GeoBasis-DE / BKG, Deutsche Post Direkt GmbH, Statistisches Bundesamt, Wiesbaden (2021)"
  )

  clean_data <- sf::st_as_sf(
    clean_data,
    coords = c("x", "y"),
    crs = 25832,
    remove = TRUE,
    na.fail = FALSE
  )
  
  if ("inspire" %in% identifiers) {
    clean_data <- tibble::add_column(
      clean_data,
      Gitter_ID_1km = spt_create_inspire_ids(data = clean_data, type = "1km"),
      Gitter_ID_100m = spt_create_inspire_ids(data = clean_data, type = "100m"),
      .after = "STA"
    )
  }
    
  if ("nuts" %in% identifiers) {
    nuts <- merge(clean_data, nuts_ars, by = "KRS", all.x = TRUE)$nuts
    clean_data <- tibble::add_column(
      clean_data,
      nuts3 = nuts,
      nuts2 = substr(nuts, 1, 4),
      nuts1 = substr(nuts, 1, 3),
      .after = "STA"
    )
  }
  
  if (!"rs" %in% identifiers) {
    clean_data[c("RS", "VWG", "KRS", "RBZ", "STA", "AGS")] <- NULL
  }

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
  }

  clean_data
}


nuts_ars <- tibble::tibble(
  KRS = c("01001", "01002", "01003", "01004", 
    "01051", "01053", "01054", "01055", "01056", "01057", "01058", 
    "01059", "01060", "01061", "01062", "02000", "03101", "03102", 
    "03103", "03151", "03153", "03154", "03155", "03157", "03158", 
    "03159", "03241", "03251", "03252", "03254", "03255", "03256", 
    "03257", "03351", "03352", "03353", "03354", "03355", "03356", 
    "03357", "03358", "03359", "03360", "03361", "03401", "03402", 
    "03403", "03404", "03405", "03451", "03452", "03453", "03454", 
    "03455", "03456", "03457", "03458", "03459", "03460", "03461", 
    "03462", "04011", "04012", "05111", "05112", "05113", "05114", 
    "05116", "05117", "05119", "05120", "05122", "05124", "05154", 
    "05158", "05162", "05166", "05170", "05314", "05315", "05316", 
    "05334", "05358", "05362", "05366", "05370", "05374", "05378", 
    "05382", "05512", "05513", "05515", "05554", "05558", "05562", 
    "05566", "05570", "05711", "05754", "05758", "05762", "05766", 
    "05770", "05774", "05911", "05913", "05914", "05915", "05916", 
    "05954", "05958", "05962", "05966", "05970", "05974", "05978", 
    "06411", "06412", "06413", "06414", "06431", "06432", "06433", 
    "06434", "06435", "06436", "06437", "06438", "06439", "06440", 
    "06531", "06532", "06533", "06534", "06535", "06611", "06631", 
    "06632", "06633", "06634", "06635", "06636", "07111", "07131", 
    "07132", "07133", "07134", "07135", "07137", "07138", "07140", 
    "07141", "07143", "07211", "07231", "07232", "07233", "07235", 
    "07311", "07312", "07313", "07314", "07315", "07316", "07317", 
    "07318", "07319", "07320", "07331", "07332", "07333", "07334", 
    "07335", "07336", "07337", "07338", "07339", "07340", "08111", 
    "08115", "08116", "08117", "08118", "08119", "08121", "08125", 
    "08126", "08127", "08128", "08135", "08136", "08211", "08212", 
    "08215", "08216", "08221", "08222", "08225", "08226", "08231", 
    "08235", "08236", "08237", "08311", "08315", "08316", "08317", 
    "08325", "08326", "08327", "08335", "08336", "08337", "08415", 
    "08416", "08417", "08421", "08425", "08426", "08435", "08436", 
    "08437", "09161", "09162", "09163", "09171", "09172", "09173", 
    "09174", "09175", "09176", "09177", "09178", "09179", "09180", 
    "09181", "09182", "09183", "09184", "09185", "09186", "09187", 
    "09188", "09189", "09190", "09261", "09262", "09263", "09271", 
    "09272", "09273", "09274", "09275", "09276", "09277", "09278", 
    "09279", "09361", "09362", "09363", "09371", "09372", "09373", 
    "09374", "09375", "09376", "09377", "09461", "09462", "09463", 
    "09464", "09471", "09472", "09473", "09474", "09475", "09476", 
    "09477", "09478", "09479", "09561", "09562", "09563", "09564", 
    "09565", "09571", "09572", "09573", "09574", "09575", "09576", 
    "09577", "09661", "09662", "09663", "09671", "09672", "09673", 
    "09674", "09675", "09676", "09677", "09678", "09679", "09761", 
    "09762", "09763", "09764", "09771", "09772", "09773", "09774", 
    "09775", "09776", "09777", "09778", "09779", "09780", "10041", 
    "10042", "10043", "10044", "10045", "10046", "11000", "12051", 
    "12052", "12053", "12054", "12060", "12061", "12062", "12063", 
    "12064", "12065", "12066", "12067", "12068", "12069", "12070", 
    "12071", "12072", "12073", "13003", "13004", "13071", "13072", 
    "13073", "13074", "13075", "13076", "14511", "14521", "14522", 
    "14523", "14524", "14612", "14625", "14626", "14627", "14628", 
    "14713", "14729", "14730", "15001", "15002", "15003", "15081", 
    "15082", "15083", "15084", "15085", "15086", "15087", "15088", 
    "15089", "15090", "15091", "16051", "16052", "16053", "16054", 
    "16055", "16056", "16061", "16062", "16063", "16064", "16065", 
    "16066", "16067", "16068", "16069", "16070", "16071", "16072", 
    "16073", "16074", "16075", "16076", "16077"),
  nuts = c("DEF01", 
    "DEF02", "DEF03", "DEF04", "DEF05", "DEF06", "DEF07", "DEF08", 
    "DEF09", "DEF0A", "DEF0B", "DEF0C", "DEF0D", "DEF0E", "DEF0F", 
    "DE600", "DE911", "DE912", "DE913", "DE914", "DE916", "DE917", 
    "DE918", "DE91A", "DE91B", "DE91C", "DE929", "DE922", "DE923", 
    "DE925", "DE926", "DE927", "DE928", "DE931", "DE932", "DE933", 
    "DE934", "DE935", "DE936", "DE937", "DE938", "DE939", "DE93A", 
    "DE93B", "DE941", "DE942", "DE943", "DE944", "DE945", "DE946", 
    "DE947", "DE948", "DE949", "DE94A", "DE94B", "DE94C", "DE94D", 
    "DE94E", "DE94F", "DE94G", "DE94H", "DE501", "DE502", "DEA11", 
    "DEA12", "DEA13", "DEA14", "DEA15", "DEA16", "DEA17", "DEA18", 
    "DEA19", "DEA1A", "DEA1B", "DEA1C", "DEA1D", "DEA1E", "DEA1F", 
    "DEA22", "DEA23", "DEA24", "DEA2D", "DEA26", "DEA27", "DEA28", 
    "DEA29", "DEA2A", "DEA2B", "DEA2C", "DEA31", "DEA32", "DEA33", 
    "DEA34", "DEA35", "DEA36", "DEA37", "DEA38", "DEA41", "DEA42", 
    "DEA43", "DEA44", "DEA45", "DEA46", "DEA47", "DEA51", "DEA52", 
    "DEA53", "DEA54", "DEA55", "DEA56", "DEA57", "DEA58", "DEA59", 
    "DEA5A", "DEA5B", "DEA5C", "DE711", "DE712", "DE713", "DE714", 
    "DE715", "DE716", "DE717", "DE718", "DE719", "DE71A", "DE71B", 
    "DE71C", "DE71D", "DE71E", "DE721", "DE722", "DE723", "DE724", 
    "DE725", "DE731", "DE732", "DE733", "DE734", "DE735", "DE736", 
    "DE737", "DEB11", "DEB12", "DEB13", "DEB14", "DEB15", "DEB1C", 
    "DEB17", "DEB18", "DEB1D", "DEB1A", "DEB1B", "DEB21", "DEB22", 
    "DEB23", "DEB24", "DEB25", "DEB31", "DEB32", "DEB33", "DEB34", 
    "DEB35", "DEB36", "DEB37", "DEB38", "DEB39", "DEB3A", "DEB3B", 
    "DEB3C", "DEB3D", "DEB3E", "DEB3F", "DEB3G", "DEB3H", "DEB3I", 
    "DEB3J", "DEB3K", "DE111", "DE112", "DE113", "DE114", "DE115", 
    "DE116", "DE117", "DE118", "DE119", "DE11A", "DE11B", "DE11C", 
    "DE11D", "DE121", "DE122", "DE123", "DE124", "DE125", "DE126", 
    "DE127", "DE128", "DE129", "DE12A", "DE12B", "DE12C", "DE131", 
    "DE132", "DE133", "DE134", "DE135", "DE136", "DE137", "DE138", 
    "DE139", "DE13A", "DE141", "DE142", "DE143", "DE144", "DE145", 
    "DE146", "DE147", "DE148", "DE149", "DE211", "DE212", "DE213", 
    "DE214", "DE215", "DE216", "DE217", "DE218", "DE219", "DE21A", 
    "DE21B", "DE21C", "DE21D", "DE21E", "DE21F", "DE21G", "DE21H", 
    "DE21I", "DE21J", "DE21K", "DE21L", "DE21M", "DE21N", "DE221", 
    "DE222", "DE223", "DE224", "DE225", "DE226", "DE227", "DE228", 
    "DE229", "DE22A", "DE22B", "DE22C", "DE231", "DE232", "DE233", 
    "DE234", "DE235", "DE236", "DE237", "DE238", "DE239", "DE23A", 
    "DE241", "DE242", "DE243", "DE244", "DE245", "DE246", "DE247", 
    "DE248", "DE249", "DE24A", "DE24B", "DE24C", "DE24D", "DE251", 
    "DE252", "DE253", "DE254", "DE255", "DE256", "DE257", "DE258", 
    "DE259", "DE25A", "DE25B", "DE25C", "DE261", "DE262", "DE263", 
    "DE264", "DE265", "DE266", "DE267", "DE268", "DE269", "DE26A", 
    "DE26B", "DE26C", "DE271", "DE272", "DE273", "DE274", "DE275", 
    "DE276", "DE277", "DE278", "DE279", "DE27A", "DE27B", "DE27C", 
    "DE27D", "DE27E", "DEC01", "DEC02", "DEC03", "DEC04", "DEC05", 
    "DEC06", "DE300", "DE401", "DE402", "DE403", "DE404", "DE405", 
    "DE406", "DE407", "DE408", "DE409", "DE40A", "DE40B", "DE40C", 
    "DE40D", "DE40E", "DE40F", "DE40G", "DE40H", "DE40I", "DE803", 
    "DE804", "DE80J", "DE80K", "DE80L", "DE80M", "DE80N", "DE80O", 
    "DED41", "DED42", "DED43", "DED44", "DED45", "DED21", "DED2C", 
    "DED2D", "DED2E", "DED2F", "DED51", "DED52", "DED53", "DEE01", 
    "DEE02", "DEE03", "DEE04", "DEE05", "DEE07", "DEE08", "DEE09", 
    "DEE06", "DEE0A", "DEE0B", "DEE0C", "DEE0D", "DEE0E", "DEG01", 
    "DEG02", "DEG03", "DEG04", "DEG05", "DEG0N", "DEG06", "DEG07", 
    "DEG0P", "DEG09", "DEG0A", "DEG0B", "DEG0C", "DEG0D", "DEG0E", 
    "DEG0F", "DEG0G", "DEG0H", "DEG0I", "DEG0J", "DEG0K", "DEG0L", 
    "DEG0M")
)