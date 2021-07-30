cellphones_data <- if(file.exists("../Data/mobile phone survey data.csv")){
    "mobile phone survey data.csv"
}

# Constant for id Var
idVal <- 'id';

# Constants for Respondent Vars
ageVal <- 'Age';
genderVal <- 'Gender';
incomeVal <- 'Income';
regionVal <- 'Region';
personaVal <- 'Persona';

# Constant for Product Var
productVal <- 'Product';

# Constants for Engagement Vars
awarenessVal <- 'Awareness';
considerationVal <- 'Consideration';
consumptionVal <- 'Consumption';
satisfactionVal <- 'Satisfaction';
advocacyVal <- 'Advocacy';

# Creating Group Values for Age & Income (which still need to be spliced)
ageGroupVal <- 'ageGroups';
incomeGroupVal <- 'incomeGroups';

# Creating Spliced Factor Levels for Group Vars for Age & Income
cellphones <- fread("../Data/mobile phone survey data.csv", verbose = F);

cellphones$ageGroups <- cut(x = cellphones$Age, breaks = c(17, 34, 49, 64, Inf, right = FALSE), labels = c('< 17 years old', '18-34 years old', '35-49 years old', '50-64 years old', '65+ years old'));
cellphones$incomeGroups <- cut(x = cellphones$Income, breaks = c(0, 49999, 74999, 99999, 149999, Inf), labels = c('<$50,000', '$50,000 - <$75,000', '$75,000 - <$100,000', '$100,000 - <$150,000', '$150,000+'));

# Ensuring Factor Levels are Unique For All Vars
ageGroups <- cellphones[ , sort(unique(get(ageGroupVal)))];
genderGroups <- cellphones[ , sort(unique(get(genderVal)))];
incomeGroups <- cellphones[ , sort(unique(get(incomeGroupVal)))];
regionGroups <- cellphones[ , sort(unique(get(regionVal)))];
personaGroups <- cellphones[ , sort(unique(get(personaVal)))];

# Ensuring Factor Levels i.e. Brands are Unique for All Product Vars
productVal <- cellphones[ , unique(get(productVal))];

# Constants for BP Vars
userFriendlyBP <- 'BP_User_Friendly_0_10';
fastBP <- 'BP_Fast_0_10';
batteryLifeBP <- 'BP_Battery_Life_0_10';
cameraBP <- 'BP_Camera_0_10';
sleekBP <- 'BP_Sleek_0_10';
stylishBP <- 'BP_Stylish_0_10';
statusSymbolBP <- 'BP_Status_Symbol_0_10';
goodScreenSizeBP <- 'BP_Good_Screen_Size_0_10';
boringBP <- 'BP_Boring_0_10';
bulkyBP <- 'BP_Bulky_0_10';
fragileBP <- 'BP_Fragile_0_10';
expensiveBP <- 'BP_Expensive_0_10';

# Constants for Product(Brand) Vars
AllButtons <- 'All Buttons';
AppMap <- 'App Map';
Buzzdial <- 'Buzzdial';
Cellularity <- 'Cellularity';
CommunicNation <- 'Communic Nation';
MaybeMobile <-  'Maybe Mobile';
MobileMayhem <- 'Mobile Mayhem';
MobilitEE <- 'MobilitEE';
Mobzilla <- 'Mobzilla';
NextText <- 'Next Text';
NoButtons <- 'No Buttons';
OfftheHook <- 'Off the Hook';
Phonatics <- 'Phonatics';
PhoneZone <- 'PhoneZone';
PocketDialz <- 'PocketDialz';
RingRing <- 'RingRing';
Screenz <- 'Screenz';
Smartophonic <- 'Smartophonic';
SpeedDials <- 'SpeedDials';
Triumphone <- 'Triumphone';

# Respondent Variables, Describe Customer Demographics and Behavioral Traits
resVars <- c(ageGroupVal, genderVal, incomeGroupVal, regionVal, personaVal);

# Engagement Variables, Cover Various Means of Ascertaining Customer Opinion
engVars <- c(awarenessVal, considerationVal, consumptionVal, satisfactionVal,
             advocacyVal);

# Brand Perception Variables (BP), Contain User Feedback/Ratings On Topics
bpVars <- c(userFriendlyBP, fastBP, batteryLifeBP, cameraBP, sleekBP, stylishBP,
            statusSymbolBP, goodScreenSizeBP, boringBP, bulkyBP, fragileBP, expensiveBP);

# Product Variables (The Different Brand / Product Types of Cell Phones)
prodVars <- c(AllButtons, AppMap, Buzzdial, Cellularity, CommunicNation,
              MaybeMobile, MobileMayhem, MobilitEE, Mobzilla, NextText, NoButtons,
              OfftheHook, Phonatics, PhoneZone, PocketDialz, RingRing, Screenz, Smartophonic,
              SpeedDials, Triumphone)
