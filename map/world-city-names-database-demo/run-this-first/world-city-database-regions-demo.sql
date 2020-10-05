#
# Table structure for table 'Cities'
#

DROP TABLE IF EXISTS `Cities`;

CREATE TABLE `Cities` (
  `CityID` INTEGER NOT NULL, 
  `ISO2` VARCHAR(2), 
  `RC` VARCHAR(2), 
  `City` VARCHAR(100), 
  `AccentCity` VARCHAR(100), 
  `Latitude` DOUBLE NULL, 
  `Longitude` DOUBLE NULL, 
  PRIMARY KEY (`CityID`)
) ENGINE=innodb DEFAULT CHARSET=utf8;

SET autocommit=1;

#
# Will dump data for table 'Cities' using separate scripts
#

#
# Table structure for table 'Continents'
#

DROP TABLE IF EXISTS `Continents`;

CREATE TABLE `Continents` (
  `Continent` VARCHAR(50) NOT NULL, 
  PRIMARY KEY (`Continent`)
) ENGINE=innodb DEFAULT CHARSET=utf8;

SET autocommit=1;

#
# Dumping data for table 'Continents'
#

INSERT INTO `Continents` VALUES ('Africa');
INSERT INTO `Continents` VALUES ('Antarctic Region');
INSERT INTO `Continents` VALUES ('Arctic Region');
INSERT INTO `Continents` VALUES ('Asia');
INSERT INTO `Continents` VALUES ('Central America and the Caribbean');
INSERT INTO `Continents` VALUES ('Commonwealth of Independent States');
INSERT INTO `Continents` VALUES ('Europe');
INSERT INTO `Continents` VALUES ('Middle East');
INSERT INTO `Continents` VALUES ('North America');
INSERT INTO `Continents` VALUES ('Oceania');
INSERT INTO `Continents` VALUES ('South America');
INSERT INTO `Continents` VALUES ('South East Asia');
INSERT INTO `Continents` VALUES ('World Others');
# 13 records

#
# Table structure for table 'Countries'
#

DROP TABLE IF EXISTS `Countries`;

CREATE TABLE `Countries` (
  `ISO2` VARCHAR(2) NOT NULL, 
  `FIPS104` VARCHAR(2), 
  `ISO3` VARCHAR(3), 
  `ISON` INTEGER, 
  `TLD` VARCHAR(3), 
  `Country` VARCHAR(50), 
  `Capital` VARCHAR(50), 
  `Continent` VARCHAR(50), 
  `NationalitySingular` VARCHAR(50), 
  `NationalityPlural` VARCHAR(50), 
  `Currency` VARCHAR(50), 
  `CurrencyCode` VARCHAR(3), 
  `Population` INTEGER, 
  PRIMARY KEY (`ISO2`)
) ENGINE=innodb DEFAULT CHARSET=utf8;

SET autocommit=1;

#
# Dumping data for table 'Countries'
#

INSERT INTO `Countries` VALUES ('AD', 'AN', 'AND', 20, '.ad', 'Andorra', 'Andorra la Vella', 'Europe', 'Andorran', 'Andorrans', 'Euro', 'EUR', 67627);
INSERT INTO `Countries` VALUES ('AE', 'AE', 'ARE', 784, '.ae', 'United Arab Emirates', 'Abu Dhabi', 'Middle East', 'Emirati', 'Emiratis', 'UAE Dirham', 'AED', 2407460);
INSERT INTO `Countries` VALUES ('AF', 'AF', 'AFG', 4, '.af', 'Afghanistan', 'Kabul', 'Asia', 'Afghan', 'Afghans', 'Afghani', 'AFA', 26813057);
INSERT INTO `Countries` VALUES ('AG', 'AC', 'ATG', 28, '.ag', 'Antigua and Barbuda', 'Saint John\'s', 'Central America and the Caribbean', 'Antiguan and Barbudan', 'Antiguans and Barbudans', 'East Caribbean Dollar', 'XCD', 66970);
INSERT INTO `Countries` VALUES ('AI', 'AV', 'AIA', 660, '.ai', 'Anguilla', 'The Valley', 'Central America and the Caribbean', 'Anguillan', 'Anguillans', 'East Caribbean Dollar', 'XCD', 12132);
INSERT INTO `Countries` VALUES ('AL', 'AL', 'ALB', 8, '.al', 'Albania', 'Tirana', 'Europe', 'Albanian', 'Albanians', 'Lek', 'ALL', 3510484);
INSERT INTO `Countries` VALUES ('AM', 'AM', 'ARM', 51, '.am', 'Armenia', 'Yerevan', 'Commonwealth of Independent States', 'Armenian', 'Armenians', 'Armenian Dram', 'AMD', 3336100);
INSERT INTO `Countries` VALUES ('AN', 'NT', 'ANT', 530, '.an', 'Netherlands Antilles', 'Willemstad', 'Central America and the Caribbean', 'Dutch Antillean', 'Dutch Antilleans', 'Netherlands Antillean guilder', 'ANG', 212226);
INSERT INTO `Countries` VALUES ('AO', 'AO', 'AGO', 24, '.ao', 'Angola', 'Luanda', 'Africa', 'Angolan', 'Angolans', 'Kwanza', 'AOA', 10366031);
INSERT INTO `Countries` VALUES ('AQ', 'AY', 'ATA', 10, '.aq', 'Antarctica', NULL, 'Antarctic Region', NULL, NULL, NULL, NULL, NULL);
INSERT INTO `Countries` VALUES ('AR', 'AR', 'ARG', 32, '.ar', 'Argentina', 'Buenos Aires', 'South America', 'Argentine', 'Argentines', 'Argentine Peso', 'ARS', 37384816);
INSERT INTO `Countries` VALUES ('AS', 'AQ', 'ASM', 16, '.as', 'American Samoa', 'Pago Pago', 'Oceania', 'American Samoan', 'American Samoans', 'US Dollar', 'USD', 67084);
INSERT INTO `Countries` VALUES ('AT', 'AU', 'AUT', 40, '.at', 'Austria', 'Vienna', 'Europe', 'Austrian', 'Austrians', 'Euro', 'EUR', 8150835);
INSERT INTO `Countries` VALUES ('AU', 'AS', 'AUS', 36, '.au', 'Australia', 'Canberra', 'Oceania', 'Australian', 'Australians', 'Australian dollar', 'AUD', 19357594);
INSERT INTO `Countries` VALUES ('AW', 'AA', 'ABW', 533, '.aw', 'Aruba', 'Oranjestad', 'Central America and the Caribbean', 'Aruban', 'Arubans', 'Aruban Guilder', 'AWG', 70007);
INSERT INTO `Countries` VALUES ('AZ', 'AJ', 'AZE', 31, '.az', 'Azerbaijan', 'Baku (Baki)', 'Commonwealth of Independent States', 'Azerbaijani', 'Azerbaijanis', 'Azerbaijani Manat', 'AZM', 7771092);
INSERT INTO `Countries` VALUES ('BA', 'BK', 'BIH', 70, '.ba', 'Bosnia and Herzegovina', 'Sarajevo', 'Europe', 'Bosnian and Herzegovinian', 'Bosnians and Herzegovinians', 'Convertible Marka', 'BAM', 3922205);
INSERT INTO `Countries` VALUES ('BB', 'BB', 'BRB', 52, '.bb', 'Barbados', 'Bridgetown', 'Central America and the Caribbean', 'Barbadian', 'Barbadians', 'Barbados Dollar', 'BBD', 275330);
INSERT INTO `Countries` VALUES ('BD', 'BG', 'BGD', 50, '.bd', 'Bangladesh', 'Dhaka', 'Asia', 'Bangladeshi', 'Bangladeshis', 'Taka', 'BDT', 131269860);
INSERT INTO `Countries` VALUES ('BE', 'BE', 'BEL', 56, '.be', 'Belgium', 'Brussels', 'Europe', 'Belgian', 'Belgians', 'Euro', 'EUR', 10258762);
INSERT INTO `Countries` VALUES ('BF', 'UV', 'BFA', 854, '.bf', 'Burkina Faso', 'Ouagadougou', 'Africa', 'Burkinabe', 'Burkinabe', 'CFA Franc BCEAO', 'XOF', 12272289);
INSERT INTO `Countries` VALUES ('BG', 'BU', 'BGR', 100, '.bg', 'Bulgaria', 'Sofia', 'Europe', 'Bulgarian', 'Bulgarians', 'Lev', 'BGN', 7707495);
INSERT INTO `Countries` VALUES ('BH', 'BA', 'BHR', 48, '.bh', 'Bahrain', 'Manama', 'Middle East', 'Bahraini', 'Bahrainis', 'Bahraini Dinar', 'BHD', 645361);
INSERT INTO `Countries` VALUES ('BI', 'BY', 'BDI', 108, '.bi', 'Burundi', 'Bujumbura', 'Africa', 'Burundi', 'Burundians', 'Burundi Franc', 'BIF', 6223897);
INSERT INTO `Countries` VALUES ('BJ', 'BN', 'BEN', 204, '.bj', 'Benin', 'Porto-Novo', 'Africa', 'Beninese', 'Beninese', 'CFA Franc BCEAO', 'XOF', 6590782);
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Countries` VALUES ('FR', 'FR', 'FRA', 250, '.fr', 'France', 'Paris', 'Europe', 'Frenchman', 'Frenchmen', 'Euro', 'EUR', 59551227);
INSERT INTO `Countries` VALUES ('GA', 'GB', 'GAB', 266, '.ga', 'Gabon', 'Libreville', 'Africa', 'Gabonese', 'Gabonese', 'CFA Franc BEAC', 'XAF', 1221175);
INSERT INTO `Countries` VALUES ('GB', 'UK', 'GBR', 826, '.uk', 'United Kingdom', 'London', 'Europe', 'British', 'Britons', 'Pound Sterling', 'GBP', 59647790);
INSERT INTO `Countries` VALUES ('GD', 'GJ', 'GRD', 308, '.gd', 'Grenada', 'Saint George\'s', 'Central America and the Caribbean', 'Grenadian', 'Grenadians', 'East Caribbean Dollar', 'XCD', 89227);
INSERT INTO `Countries` VALUES ('GE', 'GG', 'GEO', 268, '.ge', 'Georgia', 'T\'bilisi', 'Commonwealth of Independent States', 'Georgian', 'Georgians', 'Lari', 'GEL', 4989285);
INSERT INTO `Countries` VALUES ('GF', 'FG', 'GUF', 254, '.gf', 'French Guiana', 'Cayenne', 'South America', 'French Guianese', 'French Guianese', 'Euro', 'EUR', 177562);
INSERT INTO `Countries` VALUES ('GG', NULL, NULL, NULL, NULL, 'Guernsey', 'Saint Peter Port', 'Europe', NULL, NULL, NULL, NULL, NULL);
INSERT INTO `Countries` VALUES ('GH', 'GH', 'GHA', 288, '.gh', 'Ghana', 'Accra', 'Africa', 'Ghanaian', 'Ghanaians', 'Cedi', 'GHC', 19894014);
INSERT INTO `Countries` VALUES ('GI', 'GI', 'GIB', 292, '.gi', 'Gibraltar', 'Gibraltar', 'Europe', 'Gibraltar', 'Gibraltarians', 'Gibraltar Pound', 'GIP', 27649);
INSERT INTO `Countries` VALUES ('GL', 'GL', 'GRL', 304, '.gl', 'Greenland', 'Nuuk', 'Arctic Region', 'Greenlandic', 'Greenlanders', 'Danish Krone', 'DKK', 56352);
INSERT INTO `Countries` VALUES ('GM', 'GA', 'GMB', 270, '.gm', 'Gambia, The', 'Banjul', 'Africa', 'Gambian', 'Gambians', 'Dalasi', 'GMD', 1411205);
INSERT INTO `Countries` VALUES ('GN', 'GV', 'GIN', 324, '.gn', 'Guinea', 'Conakry', 'Africa', 'Guinean', 'Guineans', 'Guinean Franc', 'GNF', 7613870);
INSERT INTO `Countries` VALUES ('GP', 'GP', 'GLP', 312, '.gp', 'Guadeloupe', 'Basse-Terre', 'Central America and the Caribbean', 'Guadeloupe', 'Guadeloupians', 'Euro', 'EUR', 431170);
INSERT INTO `Countries` VALUES ('GQ', 'EK', 'GNQ', 226, '.gq', 'Equatorial Guinea', 'Malabo', 'Africa', 'Equatorial Guinean', 'Equatorial Guineans', 'CFA Franc BEAC', 'XAF', 486060);
INSERT INTO `Countries` VALUES ('GR', 'GR', 'GRC', 300, '.gr', 'Greece', 'Athens', 'Europe', 'Greek', 'Greeks', 'Euro', 'EUR', 10623835);
INSERT INTO `Countries` VALUES ('GS', 'SX', 'SGS', 239, '.gs', 'South Georgia and the Islands', NULL, 'Antarctic Region', NULL, NULL, 'Pound Sterling', 'GBP', NULL);
INSERT INTO `Countries` VALUES ('GT', 'GT', 'GTM', 320, '.gt', 'Guatemala', 'Guatemala', 'Central America and the Caribbean', 'Guatemalan', 'Guatemalans', 'Quetzal', 'GTQ', 12974361);
INSERT INTO `Countries` VALUES ('GU', 'GQ', 'GUM', 316, '.gu', 'Guam', 'Hagatna', 'Oceania', 'Guamanian', 'Guamanians', 'US Dollar', 'USD', 157557);
INSERT INTO `Countries` VALUES ('GW', 'PU', 'GNB', 624, '.gw', 'Guinea-Bissau', 'Bissau', 'Africa', 'Guinean', 'Guineans', 'CFA Franc BCEAO', 'XOF', 1315822);
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Countries` VALUES ('LI', 'LS', 'LIE', 438, '.li', 'Liechtenstein', 'Vaduz', 'Europe', 'Liechtenstein', 'Liechtensteiners', 'Swiss Franc', 'CHF', 32528);
INSERT INTO `Countries` VALUES ('LK', 'CE', 'LKA', 144, '.lk', 'Sri Lanka', 'Colombo', 'Asia', 'Sri Lankan', 'Sri Lankans', 'Sri Lanka Rupee', 'LKR', 19408635);
INSERT INTO `Countries` VALUES ('LR', 'LI', 'LBR', 430, '.lr', 'Liberia', 'Monrovia', 'Africa', 'Liberian', 'Liberians', 'Liberian Dollar', 'LRD', 3225837);
INSERT INTO `Countries` VALUES ('LS', 'LT', 'LSO', 426, '.ls', 'Lesotho', 'Maseru', 'Africa', 'Basotho', 'Mosotho', 'Loti', 'LSL', 2177062);
INSERT INTO `Countries` VALUES ('LT', 'LH', 'LTU', 440, '.lt', 'Lithuania', 'Vilnius', 'Europe', 'Lithuanian', 'Lithuanians', 'Lithuanian Litas', 'LTL', 3610535);
INSERT INTO `Countries` VALUES ('LU', 'LU', 'LUX', 442, '.lu', 'Luxembourg', 'Luxembourg', 'Europe', 'Luxembourg', 'Luxembourgers', 'Euro', 'EUR', 442972);
INSERT INTO `Countries` VALUES ('LV', 'LG', 'LVA', 428, '.lv', 'Latvia', 'Riga', 'Europe', 'Latvian', 'Latvians', 'Latvian Lats', 'LVL', 2385231);
INSERT INTO `Countries` VALUES ('LY', 'LY', 'LBY', 434, '.ly', 'Libyan Arab', 'Tripoli', 'Africa', 'Libyan', 'Libyans', 'Libyan Dinar', 'LYD', 5240599);
INSERT INTO `Countries` VALUES ('MA', 'MO', 'MAR', 504, '.ma', 'Morocco', 'Rabat', 'Africa', 'Moroccan', 'Moroccans', 'Moroccan Dirham', 'MAD', 30645305);
INSERT INTO `Countries` VALUES ('MC', 'MN', 'MCO', 492, '.mc', 'Monaco', 'Monaco', 'Europe', 'Monegasque', 'Monegasques', 'Euro', 'EUR', 31842);
INSERT INTO `Countries` VALUES ('MD', 'MD', 'MDA', 498, '.md', 'Moldova, Republic of', 'Chisinau', 'Commonwealth of Independent States', 'Moldovan', 'Moldovans', 'Moldovan Leu', 'MDL', 4431570);
INSERT INTO `Countries` VALUES ('ME', 'MJ', NULL, NULL, '.me', 'Montenegro', 'Podgorica', 'Europe', NULL, NULL, NULL, NULL, NULL);
INSERT INTO `Countries` VALUES ('MG', 'MA', 'MDG', 450, '.mg', 'Madagascar', 'Antananarivo', 'Africa', 'Malagasy', 'Malagasy', 'Malagasy Franc', 'MGF', 15982563);
INSERT INTO `Countries` VALUES ('MH', 'RM', 'MHL', 584, '.mh', 'Marshall Islands', 'Majuro', 'Oceania', 'Marshallese', 'Marshallese', 'US Dollar', 'USD', 70822);
INSERT INTO `Countries` VALUES ('MK', 'MK', 'MKD', 807, '.mk', 'The Former Yugoslav Republic of Macedonia', 'Skopje', 'Europe', 'Macedonian', 'Macedonians', 'Denar', 'MKD', 2046209);
INSERT INTO `Countries` VALUES ('ML', 'ML', 'MLI', 466, '.ml', 'Mali', 'Bamako', 'Africa', 'Malian', 'Malians', 'CFA Franc BCEAO', 'XOF', 11008518);
INSERT INTO `Countries` VALUES ('MM', 'BM', 'MMR', 104, '.mm', 'Myanmar', 'Rangoon', 'South East Asia', 'Burmese', 'Burmese', 'kyat', 'MMK', 41994678);
INSERT INTO `Countries` VALUES ('MN', 'MG', 'MNG', 496, '.mn', 'Mongolia', 'Ulaanbaatar', 'Asia', 'Mongolian', 'Mongolians', 'Tugrik', 'MNT', 2654999);
INSERT INTO `Countries` VALUES ('MO', 'MC', 'MAC', 446, '.mo', 'Macau', NULL, 'South East Asia', 'Chinese', 'Chinese', 'Pataca', 'MOP', 453733);
INSERT INTO `Countries` VALUES ('MP', 'CQ', 'MNP', 580, '.mp', 'Northern Mariana Islands', 'Saipan', 'Oceania', NULL, NULL, 'US Dollar', 'USD', 74612);
INSERT INTO `Countries` VALUES ('MQ', 'MB', 'MTQ', 474, '.mq', 'Martinique', 'Fort-de-France', 'Central America and the Caribbean', 'Martiniquais', 'Martiniquais', 'Euro', 'EUR', 418454);
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Countries` VALUES ('TF', 'FS', 'ATF', 260, '.tf', 'French Southern and Antarctic Lands', NULL, 'Antarctic Region', NULL, NULL, 'Euro', 'EUR', NULL);
INSERT INTO `Countries` VALUES ('TG', 'TO', 'TGO', 768, '.tg', 'Togo', 'Lome', 'Africa', 'Togolese', 'Togolese', 'CFA Franc BCEAO', 'XOF', 5153088);
INSERT INTO `Countries` VALUES ('TH', 'TH', 'THA', 764, '.th', 'Thailand', 'Bangkok', 'South East Asia', 'Thai', 'Thai', 'Baht', 'THB', 61797751);
INSERT INTO `Countries` VALUES ('TJ', 'TI', 'TJK', 762, '.tj', 'Tajikistan', 'Dushanbe', 'Commonwealth of Independent States', 'Tajikistani', 'Tajikistanis', 'Somoni', 'TJS', 6578681);
INSERT INTO `Countries` VALUES ('TK', 'TL', 'TKL', 772, '.tk', 'Tokelau', NULL, 'Oceania', 'Tokelauan', 'Tokelauans', 'New Zealand Dollar', 'NZD', 1445);
INSERT INTO `Countries` VALUES ('TL', 'TT', 'TLS', 626, '.tl', 'East Timor', 'Dili', 'Asia', NULL, NULL, 'Timor Escudo', 'TPE', 1040880);
INSERT INTO `Countries` VALUES ('TM', 'TX', 'TKM', 795, '.tm', 'Turkmenistan', 'Ashgabat', 'Commonwealth of Independent States', 'Turkmen', 'Turkmens', 'Manat', 'TMM', 4603244);
INSERT INTO `Countries` VALUES ('TN', 'TS', 'TUN', 788, '.tn', 'Tunisia', 'Tunis', 'Africa', 'Tunisian', 'Tunisians', 'Tunisian Dinar', 'TND', 9705102);
INSERT INTO `Countries` VALUES ('TO', 'TN', 'TON', 776, '.to', 'Tonga', 'Nuku\'alofa', 'Oceania', 'Tongan', 'Tongans', 'Pa\'anga', 'TOP', 104227);
INSERT INTO `Countries` VALUES ('TR', 'TU', 'TUR', 792, '.tr', 'Turkey', 'Ankara', 'Middle East', 'Turkish', 'Turks', 'Turkish Lira', 'TRL', 66493970);
INSERT INTO `Countries` VALUES ('TT', 'TD', 'TTO', 780, '.tt', 'Trinidad and Tobago', 'Port-of-Spain', 'Central America and the Caribbean', 'Trinidadian and Tobagonian', 'Trinidadians and Tobagonians', 'Trinidad and Tobago Dollar', 'TTD', 1169682);
INSERT INTO `Countries` VALUES ('TV', 'TV', 'TUV', 798, '.tv', 'Tuvalu', 'Funafuti', 'Oceania', 'Tuvaluan', 'Tuvaluans', 'Australian Dollar', 'AUD', 10991);
INSERT INTO `Countries` VALUES ('TW', 'TW', 'TWN', 158, '.tw', 'Taiwan', 'Taipei', 'South East Asia', 'Taiwanese', 'Taiwanese', 'New Taiwan Dollar', 'TWD', 22370461);
INSERT INTO `Countries` VALUES ('TZ', 'TZ', 'TZA', 834, '.tz', 'Tanzania, United Republic of', 'Dar es Salaam', 'Africa', 'Tanzanian', 'Tanzanians', 'Tanzanian Shilling', 'TZS', 36232074);
INSERT INTO `Countries` VALUES ('UA', 'UP', 'UKR', 804, '.ua', 'Ukraine', 'Kiev', 'Commonwealth of Independent States', 'Ukrainian', 'Ukrainians', 'Hryvnia', 'UAH', 48760474);
INSERT INTO `Countries` VALUES ('UG', 'UG', 'UGA', 800, '.ug', 'Uganda', 'Kampala', 'Africa', 'Ugandan', 'Ugandans', 'Uganda Shilling', 'UGX', 23985712);
INSERT INTO `Countries` VALUES ('UM', 'DQ', 'UMI', 581, '-', 'Jarvis Island', NULL, 'North America', NULL, NULL, 'US Dollar', 'USD', NULL);
INSERT INTO `Countries` VALUES ('US', 'US', 'USA', 840, '.us', 'United States', 'Washington, DC', 'North America', 'American', 'Americans', 'US Dollar', 'USD', 278058881);
INSERT INTO `Countries` VALUES ('UY', 'UY', 'URY', 858, '.uy', 'Uruguay', 'Montevideo', 'South America', 'Uruguayan', 'Uruguayans', 'Peso Uruguayo', 'UYU', 3360105);
INSERT INTO `Countries` VALUES ('UZ', 'UZ', 'UZB', 860, '.uz', 'Uzbekistan', 'Tashkent', 'Commonwealth of Independent States', 'Uzbekistani', 'Uzbekistanis', 'Uzbekistan Sum', 'UZS', 25155064);
INSERT INTO `Countries` VALUES ('VA', 'VT', 'VAT', 336, '.va', 'Holy See (Vatican City)', 'Vatican City', 'Europe', NULL, NULL, 'Euro', 'EUR', 890);
INSERT INTO `Countries` VALUES ('VC', 'VC', 'VCT', 670, '.vc', 'Saint Vincent and the Grenadines', 'Kingstown', 'Central America and the Caribbean', 'Saint Vincentian', 'Saint Vincentians', 'East Caribbean Dollar', 'XCD', 115942);
INSERT INTO `Countries` VALUES ('VE', 'VE', 'VEN', 862, '.ve', 'Venezuela', 'Caracas', 'South America', 'Venezuelan', 'Venezuelans', 'Bolivar', 'VEB', 23916810);
INSERT INTO `Countries` VALUES ('VG', 'VI', 'VGB', 92, '.vg', 'British Virgin Islands', 'Road Town', 'Central America and the Caribbean', 'British Virgin Islander', 'British Virgin Islanders', 'US Dollar', 'USD', 20812);
INSERT INTO `Countries` VALUES ('VI', 'VQ', 'VIR', 850, '.vi', 'Virgin Islands (US)', 'Charlotte Amalie', 'Central America and the Caribbean', 'Virgin Islander', 'Virgin Islanders', 'US Dollar', 'USD', 122211);
INSERT INTO `Countries` VALUES ('VN', 'VM', 'VNM', 704, '.vn', 'Vietnam', 'Hanoi', 'South East Asia', 'Vietnamese', 'Vietnamese', 'Dong', 'VND', 79939014);
INSERT INTO `Countries` VALUES ('VU', 'NH', 'VUT', 548, '.vu', 'Vanuatu', 'Port-Vila', 'Oceania', 'Ni-Vanuatu', 'Ni-Vanuatu', 'Vatu', 'VUV', 192910);
INSERT INTO `Countries` VALUES ('WF', 'WF', 'WLF', 876, '.wf', 'Wallis and Futuna', 'Mata-Utu', 'Oceania', 'Wallis and Futuna Islander', 'Wallis and Futuna Islanders', 'CFP Franc', 'XPF', 15435);
INSERT INTO `Countries` VALUES ('WS', 'WS', 'WSM', 882, '.ws', 'Samoa', 'Apia', 'Oceania', 'Samoan', 'Samoans', 'Tala', 'WST', 179058);
INSERT INTO `Countries` VALUES ('YE', 'YM', 'YEM', 887, '.ye', 'Yemen', 'Sanaa', 'Middle East', 'Yemeni', 'Yemenis', 'Yemeni Rial', 'YER', 18078035);
INSERT INTO `Countries` VALUES ('YT', 'MF', 'MYT', 175, '.yt', 'Mayotte', 'Mamoutzou', 'Africa', 'Mahorais', 'Mahorais', 'Euro', 'EUR', 163366);
INSERT INTO `Countries` VALUES ('ZA', 'SF', 'ZAF', 710, '.za', 'South Africa', 'Pretoria', 'Africa', 'South African', 'South Africans', 'Rand', 'ZAR', 43586097);
INSERT INTO `Countries` VALUES ('ZM', 'ZA', 'ZWB', 894, '.zm', 'Zambia', 'Lusaka', 'Africa', 'Zambian', 'Zambians', 'Kwacha', 'ZMK', 9770199);
INSERT INTO `Countries` VALUES ('ZR', NULL, NULL, NULL, NULL, 'Zaire', 'Kinshasa', 'Africa', NULL, NULL, NULL, NULL, NULL);
INSERT INTO `Countries` VALUES ('ZW', 'ZI', 'ZWE', 716, '.zw', 'Zimbabwe', 'Harare', 'Africa', 'Zimbabwean', 'Zimbabweans', 'Zimbabwe Dollar', 'ZWD', 11365366);
# 246 records

#
# Table structure for table 'Regions'
#

DROP TABLE IF EXISTS `Regions`;

CREATE TABLE `Regions` (
  `ISO2` VARCHAR(2) NOT NULL, 
  `RC` VARCHAR(2) NOT NULL, 
  `Region` VARCHAR(100), 
  PRIMARY KEY (`ISO2`, `RC`)
) ENGINE=innodb DEFAULT CHARSET=utf8;

SET autocommit=1;

#
# Dumping data for table 'Regions'
#

INSERT INTO `Regions` VALUES ('AD', '02', 'Canillo');
INSERT INTO `Regions` VALUES ('AD', '03', 'Encamp');
INSERT INTO `Regions` VALUES ('AD', '04', 'La Massana');
INSERT INTO `Regions` VALUES ('AD', '05', 'Ordino');
INSERT INTO `Regions` VALUES ('AD', '06', 'Sant Julia de Loria');
INSERT INTO `Regions` VALUES ('AD', '07', 'Andorra la Vella');
INSERT INTO `Regions` VALUES ('AD', '08', 'Escaldes-Engordany');
INSERT INTO `Regions` VALUES ('AE', '01', 'Abu Dhabi');
INSERT INTO `Regions` VALUES ('AE', '02', 'Ajman');
INSERT INTO `Regions` VALUES ('AE', '03', 'Dubai');
INSERT INTO `Regions` VALUES ('AE', '04', 'Fujairah');
INSERT INTO `Regions` VALUES ('AE', '05', 'Ras Al Khaimah');
INSERT INTO `Regions` VALUES ('AE', '06', 'Sharjah');
INSERT INTO `Regions` VALUES ('AE', '07', 'Umm Al Quwain');
INSERT INTO `Regions` VALUES ('AF', '01', 'Badakhshan');
INSERT INTO `Regions` VALUES ('AF', '02', 'Badghis');
INSERT INTO `Regions` VALUES ('AF', '03', 'Baghlan');
INSERT INTO `Regions` VALUES ('AF', '05', 'Bamian');
INSERT INTO `Regions` VALUES ('AF', '06', 'Farah');
INSERT INTO `Regions` VALUES ('AF', '07', 'Faryab');
INSERT INTO `Regions` VALUES ('AF', '08', 'Ghazni');
INSERT INTO `Regions` VALUES ('AF', '09', 'Ghowr');
INSERT INTO `Regions` VALUES ('AF', '10', 'Helmand');
INSERT INTO `Regions` VALUES ('AF', '11', 'Herat');
INSERT INTO `Regions` VALUES ('AF', '13', 'Kabol');
INSERT INTO `Regions` VALUES ('AF', '14', 'Kapisa');
INSERT INTO `Regions` VALUES ('AF', '15', NULL);
INSERT INTO `Regions` VALUES ('AF', '16', NULL);
INSERT INTO `Regions` VALUES ('AF', '17', 'Lowgar');
INSERT INTO `Regions` VALUES ('AF', '18', 'Nangarhar');
INSERT INTO `Regions` VALUES ('AF', '19', 'Nimruz');
INSERT INTO `Regions` VALUES ('AF', '20', NULL);
INSERT INTO `Regions` VALUES ('AF', '21', NULL);
INSERT INTO `Regions` VALUES ('AF', '22', NULL);
INSERT INTO `Regions` VALUES ('AF', '23', 'Kandahar');
INSERT INTO `Regions` VALUES ('AF', '24', 'Kondoz');
INSERT INTO `Regions` VALUES ('AF', '26', 'Takhar');
INSERT INTO `Regions` VALUES ('AF', '27', 'Vardak');
INSERT INTO `Regions` VALUES ('AF', '28', 'Zabol');
INSERT INTO `Regions` VALUES ('AF', '29', 'Paktika');
INSERT INTO `Regions` VALUES ('AF', '30', 'Balkh');
INSERT INTO `Regions` VALUES ('AF', '31', 'Jowzjan');
INSERT INTO `Regions` VALUES ('AF', '32', 'Samangan');
INSERT INTO `Regions` VALUES ('AF', '33', 'Sar-e Pol');
INSERT INTO `Regions` VALUES ('AF', '34', 'Konar');
INSERT INTO `Regions` VALUES ('AF', '35', 'Laghman');
INSERT INTO `Regions` VALUES ('AF', '36', 'Paktia');
INSERT INTO `Regions` VALUES ('AF', '37', 'Khowst');
INSERT INTO `Regions` VALUES ('AF', '38', 'Nurestan');
INSERT INTO `Regions` VALUES ('AF', '39', 'Oruzgan');
INSERT INTO `Regions` VALUES ('AF', '40', 'Parvan');
INSERT INTO `Regions` VALUES ('AF', '41', 'Daykondi');
INSERT INTO `Regions` VALUES ('AF', '42', 'Panjshir');
INSERT INTO `Regions` VALUES ('AG', '01', 'Barbuda');
INSERT INTO `Regions` VALUES ('AG', '03', 'Saint George');
INSERT INTO `Regions` VALUES ('AG', '04', 'Saint John');
INSERT INTO `Regions` VALUES ('AG', '05', 'Saint Mary');
INSERT INTO `Regions` VALUES ('AG', '06', 'Saint Paul');
INSERT INTO `Regions` VALUES ('AG', '07', 'Saint Peter');
INSERT INTO `Regions` VALUES ('AG', '08', 'Saint Philip');
INSERT INTO `Regions` VALUES ('AG', '09', 'Redonda');
INSERT INTO `Regions` VALUES ('AI', '00', NULL);
INSERT INTO `Regions` VALUES ('AL', '40', 'Berat');
INSERT INTO `Regions` VALUES ('AL', '41', 'Diber');
INSERT INTO `Regions` VALUES ('AL', '42', 'Durres');
INSERT INTO `Regions` VALUES ('AL', '43', 'Elbasan');
INSERT INTO `Regions` VALUES ('AL', '44', 'Fier');
INSERT INTO `Regions` VALUES ('AL', '45', 'Gjirokaster');
INSERT INTO `Regions` VALUES ('AL', '46', 'Korce');
INSERT INTO `Regions` VALUES ('AL', '47', 'Kukes');
INSERT INTO `Regions` VALUES ('AL', '48', 'Lezhe');
INSERT INTO `Regions` VALUES ('AL', '49', 'Shkoder');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('AZ', '21', 'Goranboy');
INSERT INTO `Regions` VALUES ('AZ', '22', 'Goycay');
INSERT INTO `Regions` VALUES ('AZ', '23', 'Haciqabul');
INSERT INTO `Regions` VALUES ('AZ', '24', 'Imisli');
INSERT INTO `Regions` VALUES ('AZ', '25', 'Ismayilli');
INSERT INTO `Regions` VALUES ('AZ', '26', 'Kalbacar');
INSERT INTO `Regions` VALUES ('AZ', '27', 'Kurdamir');
INSERT INTO `Regions` VALUES ('AZ', '28', 'Lacin');
INSERT INTO `Regions` VALUES ('AZ', '29', 'Lankaran');
INSERT INTO `Regions` VALUES ('AZ', '30', 'Lankaran');
INSERT INTO `Regions` VALUES ('AZ', '31', 'Lerik');
INSERT INTO `Regions` VALUES ('AZ', '32', 'Masalli');
INSERT INTO `Regions` VALUES ('AZ', '33', 'Mingacevir');
INSERT INTO `Regions` VALUES ('AZ', '34', 'Naftalan');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('AZ', '53', 'Siyazan');
INSERT INTO `Regions` VALUES ('AZ', '54', 'Sumqayit');
INSERT INTO `Regions` VALUES ('AZ', '55', 'Susa');
INSERT INTO `Regions` VALUES ('AZ', '56', 'Susa');
INSERT INTO `Regions` VALUES ('AZ', '57', 'Tartar');
INSERT INTO `Regions` VALUES ('AZ', '58', 'Tovuz');
INSERT INTO `Regions` VALUES ('AZ', '59', 'Ucar');
INSERT INTO `Regions` VALUES ('AZ', '60', 'Xacmaz');
INSERT INTO `Regions` VALUES ('AZ', '61', 'Xankandi');
INSERT INTO `Regions` VALUES ('AZ', '62', 'Xanlar');
INSERT INTO `Regions` VALUES ('AZ', '63', 'Xizi');
INSERT INTO `Regions` VALUES ('AZ', '64', 'Xocali');
INSERT INTO `Regions` VALUES ('AZ', '65', 'Xocavand');
INSERT INTO `Regions` VALUES ('AZ', '66', 'Yardimli');
INSERT INTO `Regions` VALUES ('AZ', '67', 'Yevlax');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('BE', '04', 'Liege');
INSERT INTO `Regions` VALUES ('BE', '05', 'Limburg');
INSERT INTO `Regions` VALUES ('BE', '06', 'Luxembourg');
INSERT INTO `Regions` VALUES ('BE', '07', 'Namur');
INSERT INTO `Regions` VALUES ('BE', '08', 'Oost-Vlaanderen');
INSERT INTO `Regions` VALUES ('BE', '09', 'West-Vlaanderen');
INSERT INTO `Regions` VALUES ('BE', '10', 'Brabant Wallon');
INSERT INTO `Regions` VALUES ('BE', '11', 'Brussels Hoofdstedelijk Gewest');
INSERT INTO `Regions` VALUES ('BE', '12', 'Vlaams-Brabant');
INSERT INTO `Regions` VALUES ('BF', '15', 'Bam');
INSERT INTO `Regions` VALUES ('BF', '16', NULL);
INSERT INTO `Regions` VALUES ('BF', '18', NULL);
INSERT INTO `Regions` VALUES ('BF', '19', 'Boulkiemde');
INSERT INTO `Regions` VALUES ('BF', '20', 'Ganzourgou');
INSERT INTO `Regions` VALUES ('BF', '21', 'Gnagna');
INSERT INTO `Regions` VALUES ('BF', '22', NULL);
INSERT INTO `Regions` VALUES ('BF', '24', NULL);
INSERT INTO `Regions` VALUES ('BF', '28', 'Kouritenga');
INSERT INTO `Regions` VALUES ('BF', '30', NULL);
INSERT INTO `Regions` VALUES ('BF', '31', NULL);
INSERT INTO `Regions` VALUES ('BF', '32', NULL);
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('BF', '65', 'Naouri');
INSERT INTO `Regions` VALUES ('BF', '66', 'Nayala');
INSERT INTO `Regions` VALUES ('BF', '67', 'Noumbiel');
INSERT INTO `Regions` VALUES ('BF', '68', 'Oubritenga');
INSERT INTO `Regions` VALUES ('BF', '69', 'Poni');
INSERT INTO `Regions` VALUES ('BF', '70', 'Sanmatenga');
INSERT INTO `Regions` VALUES ('BF', '71', 'Seno');
INSERT INTO `Regions` VALUES ('BF', '72', 'Sissili');
INSERT INTO `Regions` VALUES ('BF', '73', 'Sourou');
INSERT INTO `Regions` VALUES ('BF', '74', 'Tuy');
INSERT INTO `Regions` VALUES ('BF', '75', 'Yagha');
INSERT INTO `Regions` VALUES ('BF', '76', 'Yatenga');
INSERT INTO `Regions` VALUES ('BF', '77', 'Ziro');
INSERT INTO `Regions` VALUES ('BF', '78', 'Zondoma');
INSERT INTO `Regions` VALUES ('BG', '33', 'Mikhaylovgrad');
INSERT INTO `Regions` VALUES ('BG', '38', 'Blagoevgrad');
INSERT INTO `Regions` VALUES ('BG', '39', 'Burgas');
INSERT INTO `Regions` VALUES ('BG', '40', 'Dobrich');
INSERT INTO `Regions` VALUES ('BG', '41', 'Gabrovo');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('BH', '10', 'Al Mintaqah ash Shamaliyah');
INSERT INTO `Regions` VALUES ('BH', '11', 'Al Mintaqah al Wusta');
INSERT INTO `Regions` VALUES ('BH', '12', 'Madinat');
INSERT INTO `Regions` VALUES ('BH', '13', 'Ar Rifa');
INSERT INTO `Regions` VALUES ('BH', '14', 'Madinat Hamad');
INSERT INTO `Regions` VALUES ('BH', '15', 'Al Muharraq');
INSERT INTO `Regions` VALUES ('BH', '16', 'Al Asimah');
INSERT INTO `Regions` VALUES ('BH', '17', 'Al Janubiyah');
INSERT INTO `Regions` VALUES ('BH', '18', 'Ash Shamaliyah');
INSERT INTO `Regions` VALUES ('BH', '19', 'Al Wusta');
INSERT INTO `Regions` VALUES ('BI', '02', 'Bujumbura');
INSERT INTO `Regions` VALUES ('BI', '05', NULL);
INSERT INTO `Regions` VALUES ('BI', '09', 'Bubanza');
INSERT INTO `Regions` VALUES ('BI', '10', 'Bururi');
INSERT INTO `Regions` VALUES ('BI', '11', 'Cankuzo');
INSERT INTO `Regions` VALUES ('BI', '12', 'Cibitoke');
INSERT INTO `Regions` VALUES ('BI', '13', 'Gitega');
INSERT INTO `Regions` VALUES ('BI', '14', 'Karuzi');
INSERT INTO `Regions` VALUES ('BI', '15', 'Kayanza');
INSERT INTO `Regions` VALUES ('BI', '16', 'Kirundo');
INSERT INTO `Regions` VALUES ('BI', '17', 'Makamba');
INSERT INTO `Regions` VALUES ('BI', '18', 'Muyinga');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('BS', '27', 'Governor\'s Harbour');
INSERT INTO `Regions` VALUES ('BS', '28', 'Green Turtle Cay');
INSERT INTO `Regions` VALUES ('BS', '29', 'High Rock');
INSERT INTO `Regions` VALUES ('BS', '30', 'Kemps Bay');
INSERT INTO `Regions` VALUES ('BS', '31', 'Marsh Harbour');
INSERT INTO `Regions` VALUES ('BS', '32', 'Nichollstown and Berry Islands');
INSERT INTO `Regions` VALUES ('BS', '33', 'Rock Sound');
INSERT INTO `Regions` VALUES ('BS', '34', 'Sandy Point');
INSERT INTO `Regions` VALUES ('BS', '35', 'San Salvador and Rum Cay');
INSERT INTO `Regions` VALUES ('BT', '05', 'Bumthang');
INSERT INTO `Regions` VALUES ('BT', '06', 'Chhukha');
INSERT INTO `Regions` VALUES ('BT', '07', 'Chirang');
INSERT INTO `Regions` VALUES ('BT', '08', 'Daga');
INSERT INTO `Regions` VALUES ('BT', '09', 'Geylegphug');
INSERT INTO `Regions` VALUES ('BT', '10', 'Ha');
INSERT INTO `Regions` VALUES ('BT', '11', 'Lhuntshi');
INSERT INTO `Regions` VALUES ('BT', '12', 'Mongar');
INSERT INTO `Regions` VALUES ('BT', '13', 'Paro');
INSERT INTO `Regions` VALUES ('BT', '14', 'Pemagatsel');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('MA', '41', NULL);
INSERT INTO `Regions` VALUES ('MA', '42', NULL);
INSERT INTO `Regions` VALUES ('MA', '43', NULL);
INSERT INTO `Regions` VALUES ('MA', '45', 'Grand Casablanca');
INSERT INTO `Regions` VALUES ('MA', '46', 'Fes-Boulemane');
INSERT INTO `Regions` VALUES ('MA', '47', 'Marrakech-Tensift-Al Haouz');
INSERT INTO `Regions` VALUES ('MA', '48', 'Meknes-Tafilalet');
INSERT INTO `Regions` VALUES ('MA', '49', 'Rabat-Sale-Zemmour-Zaer');
INSERT INTO `Regions` VALUES ('MA', '50', 'Chaouia-Ouardigha');
INSERT INTO `Regions` VALUES ('MA', '51', 'Doukkala-Abda');
INSERT INTO `Regions` VALUES ('MA', '52', 'Gharb-Chrarda-Beni Hssen');
INSERT INTO `Regions` VALUES ('MA', '53', 'Guelmim-Es Smara');
INSERT INTO `Regions` VALUES ('MA', '54', 'Oriental');
INSERT INTO `Regions` VALUES ('MA', '55', 'Souss-Massa-Dr,a');
INSERT INTO `Regions` VALUES ('MA', '56', 'Tadla-Azilal');
INSERT INTO `Regions` VALUES ('MA', '57', 'Tanger-Tetouan');
INSERT INTO `Regions` VALUES ('MA', '58', 'Taza-Al Hoceima-Taounate');
INSERT INTO `Regions` VALUES ('MA', '59', 'La,youne-Boujdour-Sakia El Hamra');
INSERT INTO `Regions` VALUES ('MC', '00', NULL);
INSERT INTO `Regions` VALUES ('MC', '01', 'La Condamine');
INSERT INTO `Regions` VALUES ('MC', '02', 'Monaco');
INSERT INTO `Regions` VALUES ('MC', '03', 'Monte-Carlo');
INSERT INTO `Regions` VALUES ('MD', '46', NULL);
INSERT INTO `Regions` VALUES ('MD', '47', NULL);
INSERT INTO `Regions` VALUES ('MD', '48', NULL);
INSERT INTO `Regions` VALUES ('MD', '49', NULL);
INSERT INTO `Regions` VALUES ('MD', '50', NULL);
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('TG', '10', NULL);
INSERT INTO `Regions` VALUES ('TG', '11', NULL);
INSERT INTO `Regions` VALUES ('TG', '12', NULL);
INSERT INTO `Regions` VALUES ('TG', '13', NULL);
INSERT INTO `Regions` VALUES ('TG', '14', NULL);
INSERT INTO `Regions` VALUES ('TG', '16', NULL);
INSERT INTO `Regions` VALUES ('TG', '17', NULL);
INSERT INTO `Regions` VALUES ('TG', '18', NULL);
INSERT INTO `Regions` VALUES ('TG', '19', NULL);
INSERT INTO `Regions` VALUES ('TG', '21', NULL);
INSERT INTO `Regions` VALUES ('TG', '22', 'Centrale');
INSERT INTO `Regions` VALUES ('TG', '23', 'Kara');
INSERT INTO `Regions` VALUES ('TG', '24', 'Maritime');
INSERT INTO `Regions` VALUES ('TG', '25', 'Plateaux');
INSERT INTO `Regions` VALUES ('TG', '26', 'Savanes');
INSERT INTO `Regions` VALUES ('TH', '01', 'Mae Hong Son');
INSERT INTO `Regions` VALUES ('TH', '02', 'Chiang Mai');
INSERT INTO `Regions` VALUES ('TH', '03', 'Chiang Rai');
INSERT INTO `Regions` VALUES ('TH', '04', 'Nan');
INSERT INTO `Regions` VALUES ('TH', '05', 'Lamphun');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('VN', '29', NULL);
INSERT INTO `Regions` VALUES ('VN', '30', 'Quang Ninh');
INSERT INTO `Regions` VALUES ('VN', '31', NULL);
INSERT INTO `Regions` VALUES ('VN', '32', 'Son La');
INSERT INTO `Regions` VALUES ('VN', '33', 'Tay Ninh');
INSERT INTO `Regions` VALUES ('VN', '34', 'Thanh Hoa');
INSERT INTO `Regions` VALUES ('VN', '35', 'Thai Binh');
INSERT INTO `Regions` VALUES ('VN', '36', NULL);
INSERT INTO `Regions` VALUES ('VN', '37', 'Tien Giang');
INSERT INTO `Regions` VALUES ('VN', '38', NULL);
INSERT INTO `Regions` VALUES ('VN', '39', 'Lang Son');
INSERT INTO `Regions` VALUES ('VN', '40', NULL);
INSERT INTO `Regions` VALUES ('VN', '41', NULL);
INSERT INTO `Regions` VALUES ('VN', '43', 'An Giang');
INSERT INTO `Regions` VALUES ('VN', '44', 'Dac Lac');
INSERT INTO `Regions` VALUES ('VN', '45', 'Dong Nai');
INSERT INTO `Regions` VALUES ('VN', '46', 'Dong Thap');
INSERT INTO `Regions` VALUES ('VN', '47', 'Kien Giang');
INSERT INTO `Regions` VALUES ('VN', '48', NULL);
INSERT INTO `Regions` VALUES ('VN', '49', 'Song Be');
INSERT INTO `Regions` VALUES ('VN', '50', 'Vinh Phu');
INSERT INTO `Regions` VALUES ('VN', '51', 'Ha Noi');
INSERT INTO `Regions` VALUES ('VN', '52', 'Ho Chi Minh');
INSERT INTO `Regions` VALUES ('VN', '53', 'Ba Ria-Vung Tau');
INSERT INTO `Regions` VALUES ('VN', '54', 'Binh Dinh');
INSERT INTO `Regions` VALUES ('VN', '55', 'Binh Thuan');
# Data in this file is for demonstration only. Complete database is available in the full package
INSERT INTO `Regions` VALUES ('ZR', '01', NULL);
INSERT INTO `Regions` VALUES ('ZR', '02', NULL);
INSERT INTO `Regions` VALUES ('ZR', '03', NULL);
INSERT INTO `Regions` VALUES ('ZR', '04', NULL);
INSERT INTO `Regions` VALUES ('ZR', '05', NULL);
INSERT INTO `Regions` VALUES ('ZR', '06', NULL);
INSERT INTO `Regions` VALUES ('ZR', '07', NULL);
INSERT INTO `Regions` VALUES ('ZR', '08', NULL);
INSERT INTO `Regions` VALUES ('ZR', '09', NULL);
INSERT INTO `Regions` VALUES ('ZR', '10', NULL);
INSERT INTO `Regions` VALUES ('ZR', '11', NULL);
INSERT INTO `Regions` VALUES ('ZR', '12', NULL);
INSERT INTO `Regions` VALUES ('ZW', '00', NULL);
INSERT INTO `Regions` VALUES ('ZW', '01', 'Manicaland');
INSERT INTO `Regions` VALUES ('ZW', '02', 'Midlands');
INSERT INTO `Regions` VALUES ('ZW', '03', 'Mashonaland Central');
INSERT INTO `Regions` VALUES ('ZW', '04', 'Mashonaland East');
INSERT INTO `Regions` VALUES ('ZW', '05', 'Mashonaland West');
INSERT INTO `Regions` VALUES ('ZW', '06', 'Matabeleland North');
INSERT INTO `Regions` VALUES ('ZW', '07', 'Matabeleland South');
INSERT INTO `Regions` VALUES ('ZW', '08', 'Masvingo');
INSERT INTO `Regions` VALUES ('ZW', '09', 'Bulawayo');
INSERT INTO `Regions` VALUES ('ZW', '10', 'Harare');
# 4547 records

