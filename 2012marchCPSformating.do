* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

set more off

clear
quietly infix              ///
  int     year      1-4    ///
  long    serial    5-9    ///
  float   hwtsupp   10-19  ///
  byte    statefip  20-21  ///
  int     metarea   22-25  ///
  byte    month     26-27  ///
  byte    pernum    28-29  ///
  float   wtsupp    30-39  ///
  byte    age       40-41  ///
  byte    sex       42-42  ///
  int     race      43-45  ///
  int     educ      46-48  ///
  byte    empstat   49-50  ///
  byte    labforce  51-51  ///
  int     occ       52-55  ///
  int     ind       56-59  ///
  double  inctot    60-67  ///
  long    incwage   68-74  ///
  long    incbus    75-81  ///
  long    incfarm   82-88  ///
  using `"cps_00011.dat"'

replace hwtsupp  = hwtsupp  / 10000
replace wtsupp   = wtsupp   / 10000

format hwtsupp  %10.4f
format wtsupp   %10.4f
format inctot   %8.0f

label var year     `"Survey year"'
label var serial   `"Household serial number"'
label var hwtsupp  `"Household weight, Supplement"'
label var statefip `"State (FIPS code)"'
label var metarea  `"Metropolitan area"'
label var month    `"Month"'
label var pernum   `"Person number in sample unit"'
label var wtsupp   `"Supplement Weight"'
label var age      `"Age"'
label var sex      `"Sex"'
label var race     `"Race"'
label var educ     `"Educational attainment recode"'
label var empstat  `"Employment status"'
label var labforce `"Labor force status"'
label var occ      `"Occupation"'
label var ind      `"Industry"'
label var inctot   `"Total personal income"'
label var incwage  `"Wage and salary income"'
label var incbus   `"Non-farm business income"'
label var incfarm  `"Farm income"'

label define hwtsupp_lbl 0000000000 `"0000000000"'
label values hwtsupp hwtsupp_lbl

label define statefip_lbl 01 `"Alabama"'
label define statefip_lbl 02 `"Alaska"', add
label define statefip_lbl 04 `"Arizona"', add
label define statefip_lbl 05 `"Arkansas"', add
label define statefip_lbl 06 `"California"', add
label define statefip_lbl 08 `"Colorado"', add
label define statefip_lbl 09 `"Connecticut"', add
label define statefip_lbl 10 `"Delaware"', add
label define statefip_lbl 11 `"District of Columbia"', add
label define statefip_lbl 12 `"Florida"', add
label define statefip_lbl 13 `"Georgia"', add
label define statefip_lbl 15 `"Hawaii"', add
label define statefip_lbl 16 `"Idaho"', add
label define statefip_lbl 17 `"Illinois"', add
label define statefip_lbl 18 `"Indiana"', add
label define statefip_lbl 19 `"Iowa"', add
label define statefip_lbl 20 `"Kansas"', add
label define statefip_lbl 21 `"Kentucky"', add
label define statefip_lbl 22 `"Louisiana"', add
label define statefip_lbl 23 `"Maine"', add
label define statefip_lbl 24 `"Maryland"', add
label define statefip_lbl 25 `"Massachusetts"', add
label define statefip_lbl 26 `"Michigan"', add
label define statefip_lbl 27 `"Minnesota"', add
label define statefip_lbl 28 `"Mississippi"', add
label define statefip_lbl 29 `"Missouri"', add
label define statefip_lbl 30 `"Montana"', add
label define statefip_lbl 31 `"Nebraska"', add
label define statefip_lbl 32 `"Nevada"', add
label define statefip_lbl 33 `"New Hampshire"', add
label define statefip_lbl 34 `"New Jersey"', add
label define statefip_lbl 35 `"New Mexico"', add
label define statefip_lbl 36 `"New York"', add
label define statefip_lbl 37 `"North Carolina"', add
label define statefip_lbl 38 `"North Dakota"', add
label define statefip_lbl 39 `"Ohio"', add
label define statefip_lbl 40 `"Oklahoma"', add
label define statefip_lbl 41 `"Oregon"', add
label define statefip_lbl 42 `"Pennsylvania"', add
label define statefip_lbl 44 `"Rhode Island"', add
label define statefip_lbl 45 `"South Carolina"', add
label define statefip_lbl 46 `"South Dakota"', add
label define statefip_lbl 47 `"Tennessee"', add
label define statefip_lbl 48 `"Texas"', add
label define statefip_lbl 49 `"Utah"', add
label define statefip_lbl 50 `"Vermont"', add
label define statefip_lbl 51 `"Virginia"', add
label define statefip_lbl 53 `"Washington"', add
label define statefip_lbl 54 `"West Virginia"', add
label define statefip_lbl 55 `"Wisconsin"', add
label define statefip_lbl 56 `"Wyoming"', add
label define statefip_lbl 61 `"Maine-New Hampshire-Vermont"', add
label define statefip_lbl 65 `"Montana-Idaho-Wyoming"', add
label define statefip_lbl 68 `"Alaska-Hawaii"', add
label define statefip_lbl 69 `"Nebraska-North Dakota-South Dakota"', add
label define statefip_lbl 70 `"Maine-Massachusetts-New Hampshire-Rhode Island-Vermont"', add
label define statefip_lbl 71 `"Michigan-Wisconsin"', add
label define statefip_lbl 72 `"Minnesota-Iowa"', add
label define statefip_lbl 73 `"Nebraska-North Dakota-South Dakota-Kansas"', add
label define statefip_lbl 74 `"Delaware-Virginia"', add
label define statefip_lbl 75 `"North Carolina-South Carolina"', add
label define statefip_lbl 76 `"Alabama-Mississippi"', add
label define statefip_lbl 77 `"Arkansas-Oklahoma"', add
label define statefip_lbl 78 `"Arizona-New Mexico-Colorado"', add
label define statefip_lbl 79 `"Idaho-Wyoming-Utah-Montana-Nevada"', add
label define statefip_lbl 80 `"Alaska-Washington-Hawaii"', add
label define statefip_lbl 81 `"New Hampshire-Maine-Vermont-Rhode Island"', add
label define statefip_lbl 83 `"South Carolina-Georgia"', add
label define statefip_lbl 84 `"Kentucky-Tennessee"', add
label define statefip_lbl 85 `"Arkansas-Louisiana-Oklahoma"', add
label define statefip_lbl 87 `"Iowa-N Dakota-S Dakota-Nebraska-Kansas-Minnesota-Missouri"', add
label define statefip_lbl 88 `"Washington-Oregon-Alaska-Hawaii"', add
label define statefip_lbl 89 `"Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona"', add
label define statefip_lbl 90 `"Delaware-Maryland-Virginia-West Virginia"', add
label define statefip_lbl 99 `"State not identified"', add
label values statefip statefip_lbl

label define metarea_lbl 0080 `"Akron, OH"'
label define metarea_lbl 0120 `"Albany, GA"', add
label define metarea_lbl 0160 `"Albany-Schenectady-Troy, NY"', add
label define metarea_lbl 0200 `"Albuquerque, NM"', add
label define metarea_lbl 0240 `"Allentown-Bethlehem-Easton, PA/NJ"', add
label define metarea_lbl 0280 `"Altoona, PA MSA"', add
label define metarea_lbl 0320 `"Amarillo, TX"', add
label define metarea_lbl 0380 `"Anchorage, AK"', add
label define metarea_lbl 0400 `"Anderson, IN"', add
label define metarea_lbl 0440 `"Ann Arbor, MI"', add
label define metarea_lbl 0450 `"Anniston, AL"', add
label define metarea_lbl 0451 `"Anniston-Oxford, AL"', add
label define metarea_lbl 0460 `"Appleton,Oshkosh-Neenah, WI"', add
label define metarea_lbl 0461 `"Appleton, WI"', add
label define metarea_lbl 0462 `"Oshkosh-Neenah, WI"', add
label define metarea_lbl 0480 `"Asheville, NC"', add
label define metarea_lbl 0500 `"Athens, GA"', add
label define metarea_lbl 0501 `"Athens-Clark County, GA"', add
label define metarea_lbl 0520 `"Atlanta, GA"', add
label define metarea_lbl 0521 `"Atlanta-Sandy Springs-Marietta, GA"', add
label define metarea_lbl 0560 `"Atlantic City, NJ"', add
label define metarea_lbl 0600 `"Augusta-Aiken, GA-SC"', add
label define metarea_lbl 0601 `"Augusta-Richmond County, GA-SC"', add
label define metarea_lbl 0640 `"Austin, TX"', add
label define metarea_lbl 0641 `"Austin-Round Rock, TX"', add
label define metarea_lbl 0680 `"Bakersfield, CA"', add
label define metarea_lbl 0720 `"Baltimore, MD"', add
label define metarea_lbl 0721 `"Baltimore-Towson, MD"', add
label define metarea_lbl 0730 `"Bangor, ME"', add
label define metarea_lbl 0740 `"Barnstable-Yarmouth, MA"', add
label define metarea_lbl 0741 `"Barnstable Town, MA"', add
label define metarea_lbl 0760 `"Baton Rouge, LA"', add
label define metarea_lbl 0780 `"Battle Creek, MI"', add
label define metarea_lbl 0840 `"Beaumont-Port Arthur-Orange, TX"', add
label define metarea_lbl 0841 `"Beaumont-Port Arthur, TX"', add
label define metarea_lbl 0860 `"Bellingham, WA"', add
label define metarea_lbl 0870 `"Benton Harbor, MI"', add
label define metarea_lbl 0871 `"Niles-Benton Harbor, MI"', add
label define metarea_lbl 0880 `"Billings, MT"', add
label define metarea_lbl 0900 `"Bend, OR"', add
label define metarea_lbl 0920 `"Biloxi-Gulfport, MS"', add
label define metarea_lbl 0960 `"Binghamton, NY"', add
label define metarea_lbl 1000 `"Birmingham, AL"', add
label define metarea_lbl 1001 `"Birmingham-Hoover, AL"', add
label define metarea_lbl 1020 `"Bloomington, IN"', add
label define metarea_lbl 1040 `"Bloomington-Normal, IL"', add
label define metarea_lbl 1080 `"Boise City, ID"', add
label define metarea_lbl 1081 `"Boise City-Nampa, ID"', add
label define metarea_lbl 1120 `"Boston, MA"', add
label define metarea_lbl 1121 `"Lawrence-Haverhill. MA/NH"', add
label define metarea_lbl 1122 `"Lowell, MA/NH"', add
label define metarea_lbl 1123 `"Salem-Gloucester, MA"', add
label define metarea_lbl 1124 `"Boston-Cambridge-Quincy, MA-NH"', add
label define metarea_lbl 1130 `"Bowling Green, KY"', add
label define metarea_lbl 1140 `"Bradenton, FL"', add
label define metarea_lbl 1150 `"Bremerton-Silverdale, WA"', add
label define metarea_lbl 1160 `"Bridgeport, CT"', add
label define metarea_lbl 1161 `"Bridgeport-Stamford-Norwalk, CT"', add
label define metarea_lbl 1200 `"Brockton, MA"', add
label define metarea_lbl 1240 `"Brownsville-Harlingen-San Benito,TX"', add
label define metarea_lbl 1241 `"Brownsville-Harlingen, TX"', add
label define metarea_lbl 1280 `"Buffalo-Niagara Falls, NY"', add
label define metarea_lbl 1281 `"Niagara Falls, NY"', add
label define metarea_lbl 1310 `"Burlington, VT"', add
label define metarea_lbl 1311 `"Burlington-South Burlington, VT"', add
label define metarea_lbl 1320 `"Canton, OH"', add
label define metarea_lbl 1321 `"Canton-Massillon, OH"', add
label define metarea_lbl 1360 `"Cedar Rapids, IA"', add
label define metarea_lbl 1400 `"Champaign-Urbana-Rantoul, IL"', add
label define metarea_lbl 1401 `"Champaign-Urbana, IL"', add
label define metarea_lbl 1440 `"Charleston-North Charleston, SC"', add
label define metarea_lbl 1480 `"Charleston, WV"', add
label define metarea_lbl 1520 `"Charlotte-Gastonia-Rock Hill, NC/SC"', add
label define metarea_lbl 1521 `"Charlotte-Gastonia-Concord, NC/SC"', add
label define metarea_lbl 1560 `"Chattanooga, TN/GA"', add
label define metarea_lbl 1600 `"Chicago-Gary-Lake IL"', add
label define metarea_lbl 1601 `"Aurora-Elgin, IL"', add
label define metarea_lbl 1602 `"Gary-Hamond-East Chicago, IN"', add
label define metarea_lbl 1603 `"Joliet, IL"', add
label define metarea_lbl 1604 `"Lake County, IL"', add
label define metarea_lbl 1605 `"Chicago-Naperville-Joliet, IL-IN-WI"', add
label define metarea_lbl 1620 `"Chico,CA"', add
label define metarea_lbl 1640 `"Cincinnati-Hamilton,OH/KY/IN"', add
label define metarea_lbl 1641 `"Cincinnati-Middleton, OH/KY/IN"', add
label define metarea_lbl 1660 `"Clarksville-Hopkinsville,TN/KY"', add
label define metarea_lbl 1661 `"Clarksville, TN/KY, TN/KY"', add
label define metarea_lbl 1680 `"Cleveland, OH"', add
label define metarea_lbl 1681 `"Cleveland-Lorain-Mentor, OH"', add
label define metarea_lbl 1700 `"Coeur d'Alene, ID"', add
label define metarea_lbl 1720 `"Colorado Springs, CO"', add
label define metarea_lbl 1740 `"Columbia, MO"', add
label define metarea_lbl 1760 `"Columbia, SC"', add
label define metarea_lbl 1800 `"Columbus, GA/AL"', add
label define metarea_lbl 1840 `"Columbus, OH"', add
label define metarea_lbl 1880 `"Corpus Christi, TX"', add
label define metarea_lbl 1920 `"Dallas-Fort Worth, TX"', add
label define metarea_lbl 1921 `"Fort Worth-Arlington, TX"', add
label define metarea_lbl 1922 `"Dallas-Fort Worth-Arlington, TX"', add
label define metarea_lbl 1930 `"Danbury, CT"', add
label define metarea_lbl 1960 `"Davenport-Rock Island-Moline, IA/IL"', add
label define metarea_lbl 2000 `"Dayton-Springfield, OH"', add
label define metarea_lbl 2001 `"Springfield, OH"', add
label define metarea_lbl 2002 `"Dayton, OH"', add
label define metarea_lbl 2020 `"Daytona Beach, FL"', add
label define metarea_lbl 2021 `"Deltona-Daytona Beach-Ormond Beach, FL"', add
label define metarea_lbl 2030 `"Decatur, AL"', add
label define metarea_lbl 2040 `"Decatur, IL"', add
label define metarea_lbl 2080 `"Denver-Boulder-Longmont, CO"', add
label define metarea_lbl 2081 `"Boulder-Longmont, CO"', add
label define metarea_lbl 2082 `"Boulder, CO"', add
label define metarea_lbl 2083 `"Denver-Aurora, CO"', add
label define metarea_lbl 2120 `"Des Moines, IA"', add
label define metarea_lbl 2160 `"Detroit, MI"', add
label define metarea_lbl 2161 `"Detroit-Warren-Livonia, MI"', add
label define metarea_lbl 2190 `"Dover, DE"', add
label define metarea_lbl 2240 `"Duluth-Superior, MN/WI"', add
label define metarea_lbl 2241 `"Duluth, MN/WI"', add
label define metarea_lbl 2281 `"Dutchess County, NY"', add
label define metarea_lbl 2290 `"Eau Claire, WI"', add
label define metarea_lbl 2300 `"El Centro, CA"', add
label define metarea_lbl 2310 `"El Paso, TX"', add
label define metarea_lbl 2360 `"Erie, PA"', add
label define metarea_lbl 2400 `"Eugene-Springfield, OR"', add
label define metarea_lbl 2440 `"Evansville, IN/KY"', add
label define metarea_lbl 2520 `"Fargo-Moorhead, ND/MN"', add
label define metarea_lbl 2521 `"Fargo, ND/MN"', add
label define metarea_lbl 2540 `"Farmington, NM"', add
label define metarea_lbl 2560 `"Fayetteville, NC"', add
label define metarea_lbl 2580 `"Fayetteville-Springdale, AR"', add
label define metarea_lbl 2581 `"Fayetteville-Springdale-Rogers, AR-MO"', add
label define metarea_lbl 2600 `"Fitchburg-Leominster, MA"', add
label define metarea_lbl 2601 `"Leominster-Fitchburg-Gardner, MA"', add
label define metarea_lbl 2640 `"Flint, MI"', add
label define metarea_lbl 2650 `"Florence, AL"', add
label define metarea_lbl 2660 `"Florence, SC"', add
label define metarea_lbl 2670 `"Fort Collins-Loveland, CO"', add
label define metarea_lbl 2680 `"Fort Lauderdale-Hollywood-Pompano Beach, FL"', add
label define metarea_lbl 2700 `"Fort Myers-Cape Coral, FL"', add
label define metarea_lbl 2710 `"Fort Pierce, FL"', add
label define metarea_lbl 2711 `"Port St. Lucie-Fort Pierce, FL"', add
label define metarea_lbl 2720 `"Fort Smith, AR/OK"', add
label define metarea_lbl 2750 `"Fort Walton Beach, FL"', add
label define metarea_lbl 2751 `"Fort Walton Beach-Crestview-Destin, FL"', add
label define metarea_lbl 2760 `"Fort Wayne, IN"', add
label define metarea_lbl 2840 `"Fresno, CA"', add
label define metarea_lbl 2880 `"Gadsden, AL"', add
label define metarea_lbl 2900 `"Gainesville, FL"', add
label define metarea_lbl 2920 `"Galveston-Texas City, TX"', add
label define metarea_lbl 3000 `"Grand Rapids, MI"', add
label define metarea_lbl 3001 `"Grand Rapids-Wyoming, MI"', add
label define metarea_lbl 3002 `"Grand Rapids-Muskegon-Holland, MI MSA"', add
label define metarea_lbl 3003 `"Holland-Grand Haven, MI"', add
label define metarea_lbl 3060 `"Greeley, CO"', add
label define metarea_lbl 3080 `"Green Bay, WI"', add
label define metarea_lbl 3120 `"Greensboro-Winston Salem, NC"', add
label define metarea_lbl 3121 `"Winston-Salem, NC"', add
label define metarea_lbl 3122 `"Greensboro-High Point, NC"', add
label define metarea_lbl 3150 `"Greenville, NC"', add
label define metarea_lbl 3160 `"Greenville-Spartanburg-Anderson, SC"', add
label define metarea_lbl 3161 `"Anderson, SC"', add
label define metarea_lbl 3162 `"Greenville, SC"', add
label define metarea_lbl 3163 `"Spartanburg, SC"', add
label define metarea_lbl 3180 `"Hagerstown, MD"', add
label define metarea_lbl 3181 `"Hagerstown-Martinsburg, MD-WV"', add
label define metarea_lbl 3200 `"Hamilton-Middleton, OH"', add
label define metarea_lbl 3240 `"Harrisburg-Lebanon-Carlisle, PA"', add
label define metarea_lbl 3241 `"Harrisburg-Carlisle, PA"', add
label define metarea_lbl 3260 `"Harrisonburg, VA"', add
label define metarea_lbl 3280 `"Hartford-Bristol-Middleton- New Britain, CT"', add
label define metarea_lbl 3283 `"New Britain, CT"', add
label define metarea_lbl 3284 `"Hartford-West Hartford-East Hartford"', add
label define metarea_lbl 3285 `"Hartford, CT"', add
label define metarea_lbl 3290 `"Hickory-Morganton, NC"', add
label define metarea_lbl 3291 `"Hickory-Morganton-Lenoir, NC"', add
label define metarea_lbl 3320 `"Honolulu, HI"', add
label define metarea_lbl 3350 `"Houma-Thibodaux, LA"', add
label define metarea_lbl 3351 `"Houma-Bayou Cane-Thibodaux, LA"', add
label define metarea_lbl 3360 `"Houston-Brazoria,TX"', add
label define metarea_lbl 3361 `"Brazoria, TX"', add
label define metarea_lbl 3362 `"Houston-Baytown-Sugar Land, TX"', add
label define metarea_lbl 3400 `"Huntington-Ashland,WV/KY/OH"', add
label define metarea_lbl 3440 `"Huntsville,AL"', add
label define metarea_lbl 3480 `"Indianapolis, IN"', add
label define metarea_lbl 3500 `"Iowa City, IA"', add
label define metarea_lbl 3520 `"Jackson, MI"', add
label define metarea_lbl 3560 `"Jackson, MS"', add
label define metarea_lbl 3590 `"Jacksonville,FL"', add
label define metarea_lbl 3600 `"Jacksonville, NC"', add
label define metarea_lbl 3610 `"Jamestown-Dunkirk, NY"', add
label define metarea_lbl 3611 `"Jamestown, NY MSA"', add
label define metarea_lbl 3620 `"Janesville-Beloit, WI"', add
label define metarea_lbl 3621 `"Janvesville, WI"', add
label define metarea_lbl 3660 `"Johnson City-Kingsport-Bristol, TN/VA"', add
label define metarea_lbl 3661 `"Johnson City, TN"', add
label define metarea_lbl 3662 `"Kingsport-Bristol, TN-VA"', add
label define metarea_lbl 3680 `"Johnstown, PA"', add
label define metarea_lbl 3710 `"Joplin, MO"', add
label define metarea_lbl 3720 `"Kalamazoo-Portage, MI"', add
label define metarea_lbl 3721 `"Kalamazoo-Battle Creek, MI MSA"', add
label define metarea_lbl 3740 `"Kankakee, IL"', add
label define metarea_lbl 3741 `"Kankakee-Bradley, IL"', add
label define metarea_lbl 3760 `"Kansas City, MO/KS"', add
label define metarea_lbl 3810 `"Killeen-Temple,TX"', add
label define metarea_lbl 3811 `"Killeen-Temple-Fort Hood, TX"', add
label define metarea_lbl 3830 `"Kingston, NY"', add
label define metarea_lbl 3840 `"Knoxville, TN"', add
label define metarea_lbl 3870 `"LaCrosse, WI"', add
label define metarea_lbl 3880 `"Lafayette, LA"', add
label define metarea_lbl 3960 `"Lake Charles, LA"', add
label define metarea_lbl 3980 `"Lakeland-Winterhaven, FL"', add
label define metarea_lbl 4000 `"Lancaster, PA"', add
label define metarea_lbl 4040 `"Lansing-East Lansing, MI"', add
label define metarea_lbl 4080 `"Laredo, TX"', add
label define metarea_lbl 4100 `"Las Cruces, NM"', add
label define metarea_lbl 4120 `"Las Vegas, NV"', add
label define metarea_lbl 4130 `"Las Vegas-Paradise, NM"', add
label define metarea_lbl 4150 `"Lawrence, KS"', add
label define metarea_lbl 4200 `"Lawton, OK"', add
label define metarea_lbl 4280 `"Lexington-Fayette, KY"', add
label define metarea_lbl 4320 `"Lima, OH"', add
label define metarea_lbl 4360 `"Lincoln, NE"', add
label define metarea_lbl 4400 `"Little Rock-North Little Rock, AR"', add
label define metarea_lbl 4420 `"Longview-Marshall, TX"', add
label define metarea_lbl 4421 `"Longview, TX"', add
label define metarea_lbl 4440 `"Lorain-Elyria, OH"', add
label define metarea_lbl 4480 `"Los Angeles-Long Beach, CA"', add
label define metarea_lbl 4481 `"Anaheim-Santa Ana- Garden Grove, CA"', add
label define metarea_lbl 4482 `"Orange County, CA"', add
label define metarea_lbl 4483 `"Los Angeles-Long Beach-Santa Ana, CA"', add
label define metarea_lbl 4520 `"Louisville, KY/IN"', add
label define metarea_lbl 4600 `"Lubbock, TX"', add
label define metarea_lbl 4640 `"Lynchburg, VA"', add
label define metarea_lbl 4680 `"Macon-Warner Robins, GA"', add
label define metarea_lbl 4681 `"Macon, GA"', add
label define metarea_lbl 4682 `"Warner Robins, GA"', add
label define metarea_lbl 4700 `"Madera, CA"', add
label define metarea_lbl 4720 `"Madison, WI"', add
label define metarea_lbl 4760 `"Manchester, NH"', add
label define metarea_lbl 4800 `"Mansfield, OH"', add
label define metarea_lbl 4880 `"McAllen-Edinburg-Pharr-Mission, TX"', add
label define metarea_lbl 4881 `"McAllen-Edinburg-Pharr, TX"', add
label define metarea_lbl 4890 `"Medford, OR"', add
label define metarea_lbl 4900 `"Melbourne-Titusville-Cocoa-Palm Beach, FL"', add
label define metarea_lbl 4901 `"Palm Bay-Melbourne-Titusville, FL"', add
label define metarea_lbl 4920 `"Memphis, TN/AR/MS"', add
label define metarea_lbl 4940 `"Merced, CA"', add
label define metarea_lbl 5000 `"Miami-Hialeah, FL"', add
label define metarea_lbl 5001 `"Miami-Fort Lauderdale-Miami Beach, FL"', add
label define metarea_lbl 5020 `"Michigan City-La Porte, IN"', add
label define metarea_lbl 5080 `"Milwaukee, WI"', add
label define metarea_lbl 5081 `"Milwaukee-Waukesha-West Allis, WI"', add
label define metarea_lbl 5120 `"Minneapolis-St. Paul, MN"', add
label define metarea_lbl 5121 `"Minneapolis-St. Paul-Bloomington, MN/WI"', add
label define metarea_lbl 5160 `"Mobile, AL"', add
label define metarea_lbl 5170 `"Modesto, CA"', add
label define metarea_lbl 5190 `"Monmouth-Ocean, NJ"', add
label define metarea_lbl 5200 `"Monroe, LA"', add
label define metarea_lbl 5220 `"Monroe, MI"', add
label define metarea_lbl 5240 `"Montgomery, Al"', add
label define metarea_lbl 5320 `"Muskegon-Norton Shores-Muskegon Heights, MI"', add
label define metarea_lbl 5321 `"Muskegon-Norton Shores, MI"', add
label define metarea_lbl 5330 `"Myrtle Beach, SC"', add
label define metarea_lbl 5331 `"Myrtle Beach-Conway-North Myrtle Beach, SC"', add
label define metarea_lbl 5340 `"Naples, FL"', add
label define metarea_lbl 5341 `"Naples-Marco Island, FL"', add
label define metarea_lbl 5350 `"Nashua, NH"', add
label define metarea_lbl 5360 `"Nashville, TN"', add
label define metarea_lbl 5361 `"Nashville-Davidson-Murfreesboro, TN"', add
label define metarea_lbl 5400 `"New Bedford, MA"', add
label define metarea_lbl 5480 `"New Haven-Meriden, CT"', add
label define metarea_lbl 5481 `"New Haven, CT"', add
label define metarea_lbl 5520 `"New London-Norwich, CT/RI"', add
label define metarea_lbl 5560 `"New Orleans, LA"', add
label define metarea_lbl 5561 `"New Orleans-Metairie-Kenner, LA"', add
label define metarea_lbl 5600 `"New York-Northeastern NJ"', add
label define metarea_lbl 5601 `"Nassau-Suffolk, NY"', add
label define metarea_lbl 5602 `"Bergen-Passaic, NJ"', add
label define metarea_lbl 5603 `"Jersey City, NJ"', add
label define metarea_lbl 5604 `"Middlesex-Somerset-Hunterdon, NJ"', add
label define metarea_lbl 5605 `"Newark, NJ"', add
label define metarea_lbl 5606 `"New York-Northern New Jersey-Long Island, NY-NJ-PA"', add
label define metarea_lbl 5607 `"New York, NY"', add
label define metarea_lbl 5640 `"Newark, OH"', add
label define metarea_lbl 5660 `"Newburgh-Middletown, NY"', add
label define metarea_lbl 5720 `"Norfolk-Virginia Beach-Newport News, VA"', add
label define metarea_lbl 5721 `"Virginia Beach-Norfolk-Newport News, VA/NC"', add
label define metarea_lbl 5760 `"Norwalk, CT"', add
label define metarea_lbl 5790 `"Ocala, FL"', add
label define metarea_lbl 5800 `"Odessa, TX"', add
label define metarea_lbl 5801 `"Midland, TX"', add
label define metarea_lbl 5840 `"Ocean City, NJ"', add
label define metarea_lbl 5880 `"Oklahoma City, OK"', add
label define metarea_lbl 5910 `"Olympia, WA"', add
label define metarea_lbl 5920 `"Omaha, NE/IA"', add
label define metarea_lbl 5921 `"Omaha-Council Bluffs, NE/IA"', add
label define metarea_lbl 5950 `"Orange, NY"', add
label define metarea_lbl 5960 `"Orlando, FL"', add
label define metarea_lbl 6010 `"Panama City, FL"', add
label define metarea_lbl 6011 `"Panama City-Lynn Haven, FL"', add
label define metarea_lbl 6080 `"Pensacola, FL"', add
label define metarea_lbl 6081 `"Pensacola-Ferry Pass-Brent, FL"', add
label define metarea_lbl 6120 `"Peoria, IL"', add
label define metarea_lbl 6160 `"Philadelphia, PA/NJ"', add
label define metarea_lbl 6161 `"Philadelphia-Camden-Wilmington, PA/NJ/DE"', add
label define metarea_lbl 6200 `"Phoenix, AZ"', add
label define metarea_lbl 6201 `"Phoenix-Mesa-Scottsdale, AZ"', add
label define metarea_lbl 6280 `"Pittsburg, PA"', add
label define metarea_lbl 6281 `"Beaver County"', add
label define metarea_lbl 6400 `"Portland, ME"', add
label define metarea_lbl 6401 `"Portland-South Portland, ME"', add
label define metarea_lbl 6440 `"Portland-Vancouver, OR/WA"', add
label define metarea_lbl 6441 `"Vancouver, WA"', add
label define metarea_lbl 6442 `"Portland-Vancouver-Beaverton, OR/WA"', add
label define metarea_lbl 6450 `"Portsmouth-Dover-Rochester, NH/ME"', add
label define metarea_lbl 6451 `"Portsmouth-Rochester, NH/ME MSA"', add
label define metarea_lbl 6452 `"Rochester-Dover, NH/ME"', add
label define metarea_lbl 6460 `"Poughkeepsie, NY"', add
label define metarea_lbl 6461 `"Poughkeepsie-Newburgh-Middletown, NY"', add
label define metarea_lbl 6470 `"Prescott, AZ"', add
label define metarea_lbl 6480 `"Providence-Fall River-Pawtucket, MA/RI"', add
label define metarea_lbl 6482 `"Pawtuckett-Woonsocket-Attleboro, RI/MA"', add
label define metarea_lbl 6483 `"Providence-Fall River-Warwick, MA-RI"', add
label define metarea_lbl 6520 `"Provo-Orem, UT"', add
label define metarea_lbl 6560 `"Pueblo, CO"', add
label define metarea_lbl 6580 `"Punta Gorda, FL"', add
label define metarea_lbl 6600 `"Racine, WI"', add
label define metarea_lbl 6640 `"Raleigh-Durham, NC"', add
label define metarea_lbl 6641 `"Durham, NC"', add
label define metarea_lbl 6642 `"Raleigh-Carey, NC"', add
label define metarea_lbl 6680 `"Reading, PA"', add
label define metarea_lbl 6720 `"Reno, NV"', add
label define metarea_lbl 6721 `"Reno-Sparks, NV"', add
label define metarea_lbl 6760 `"Richmond-Petersburg, VA"', add
label define metarea_lbl 6761 `"Richmond, VA"', add
label define metarea_lbl 6780 `"Riverside-San Bernadino, CA"', add
label define metarea_lbl 6800 `"Roanoke, VA"', add
label define metarea_lbl 6840 `"Rochester, NY"', add
label define metarea_lbl 6880 `"Rockford, IL"', add
label define metarea_lbl 6920 `"Sacramento, CA"', add
label define metarea_lbl 6921 `"Sacramento-Arden Arcade-Roseville, CA"', add
label define metarea_lbl 6960 `"Saginaw-Bay City-Midland, MI"', add
label define metarea_lbl 6961 `"Saginaw-Saginaw Township North, MI"', add
label define metarea_lbl 6980 `"St. Cloud, MN"', add
label define metarea_lbl 7040 `"St. Louis, MO/IL"', add
label define metarea_lbl 7080 `"Salem, OR"', add
label define metarea_lbl 7120 `"Salinas-Sea Side-Monterey, CA"', add
label define metarea_lbl 7121 `"Salinas, CA"', add
label define metarea_lbl 7130 `"Salisbury, MD"', add
label define metarea_lbl 7160 `"Salt Lake City-Ogden, UT"', add
label define metarea_lbl 7161 `"Salt Lake City, UT"', add
label define metarea_lbl 7162 `"Ogden-Clearfield, UT"', add
label define metarea_lbl 7240 `"San Antonio, TX"', add
label define metarea_lbl 7320 `"San Diego, CA"', add
label define metarea_lbl 7321 `"San Diego-Carlsbad-San Marcos, CA"', add
label define metarea_lbl 7360 `"San Francisco-Oaklan-Vallejo, CA"', add
label define metarea_lbl 7361 `"Oakland, CA"', add
label define metarea_lbl 7362 `"Vallejo-Fairfield-Napa, CA"', add
label define metarea_lbl 7363 `"Vallejo-Fairfield, CA"', add
label define metarea_lbl 7364 `"Napa, CA"', add
label define metarea_lbl 7365 `"San Francisco-Oakland-Fremont, CA"', add
label define metarea_lbl 7400 `"San Jose, CA"', add
label define metarea_lbl 7401 `"San Jose-Sunnyvale-Santa Clara, CA"', add
label define metarea_lbl 7460 `"San Luis Obispo-Atascadero-Paso Robles, CA"', add
label define metarea_lbl 7461 `"San Luis Obispo-Paso Robles, CA"', add
label define metarea_lbl 7470 `"Santa Barbara-Santa Maria-Lompoc, CA"', add
label define metarea_lbl 7471 `"Santa Barbara-Santa Maria-Goleta, CA"', add
label define metarea_lbl 7480 `"Santa Cruz, CA"', add
label define metarea_lbl 7481 `"Santa Cruz-Watsonville, CA"', add
label define metarea_lbl 7490 `"Santa Fe, NM"', add
label define metarea_lbl 7500 `"Santa Rosa-Petaluma, CA"', add
label define metarea_lbl 7510 `"Sarasota, FL"', add
label define metarea_lbl 7511 `"Sarasota-Bradenton-Venice, FL"', add
label define metarea_lbl 7520 `"Savannah, GA"', add
label define metarea_lbl 7560 `"Scranton-Wilkes-Barre, PA"', add
label define metarea_lbl 7600 `"Seattle-Everett, WA"', add
label define metarea_lbl 7601 `"Seattle-Tacoma-Bellevue, WA"', add
label define metarea_lbl 7610 `"Sharon, PA"', add
label define metarea_lbl 7680 `"Shreveport, LA"', add
label define metarea_lbl 7681 `"Shreveport-Bossier City, LA"', add
label define metarea_lbl 7720 `"Sioux City, IA-NE"', add
label define metarea_lbl 7760 `"Sioux Falls, SD"', add
label define metarea_lbl 7800 `"South Bend-Mishawaka, IN"', add
label define metarea_lbl 7840 `"Spokane, WA"', add
label define metarea_lbl 7880 `"Springfield, IL"', add
label define metarea_lbl 7920 `"Springfield, MO"', add
label define metarea_lbl 8000 `"Springfield-Holyoke-Chicopee, MA"', add
label define metarea_lbl 8001 `"Springfield, MA/CT"', add
label define metarea_lbl 8040 `"Stamford, CT"', add
label define metarea_lbl 8120 `"Stockton, CA"', add
label define metarea_lbl 8160 `"Syracuse, NY"', add
label define metarea_lbl 8200 `"Tacoma, WA"', add
label define metarea_lbl 8240 `"Tallahassee, FL"', add
label define metarea_lbl 8280 `"Tampa-St. Petersburg-Clearwater, FL"', add
label define metarea_lbl 8320 `"Terre Haute, IN"', add
label define metarea_lbl 8400 `"Toledo, OH/MI"', add
label define metarea_lbl 8440 `"Topeka, KS"', add
label define metarea_lbl 8480 `"Trenton, NJ"', add
label define metarea_lbl 8481 `"Trenton-Ewing, NJ"', add
label define metarea_lbl 8520 `"Tucson, AZ"', add
label define metarea_lbl 8560 `"Tulsa, OK"', add
label define metarea_lbl 8600 `"Tuscaloosa, AL"', add
label define metarea_lbl 8680 `"Utica-Rome, NY"', add
label define metarea_lbl 8700 `"Valdosta, GA"', add
label define metarea_lbl 8730 `"Ventura-Oxnard-Simi Valley, CA"', add
label define metarea_lbl 8731 `"Oxnard-Thousand Oaks-Ventura, CA"', add
label define metarea_lbl 8740 `"Vero Beach, FL"', add
label define metarea_lbl 8750 `"Victoria, TX"', add
label define metarea_lbl 8760 `"Vineland-Milville-Bridgetown, NJ"', add
label define metarea_lbl 8780 `"Visalia-Tulare-Porterville, CA"', add
label define metarea_lbl 8781 `"Visalia-Porterville, CA"', add
label define metarea_lbl 8800 `"Waco, TX"', add
label define metarea_lbl 8840 `"Washington, DC/MD/VA"', add
label define metarea_lbl 8880 `"Waterbury, CT"', add
label define metarea_lbl 8920 `"Waterloo-Cedar Falls, IA"', add
label define metarea_lbl 8940 `"Wausau, WI"', add
label define metarea_lbl 8960 `"West Palm Beach-Boca Raton-Delray Beach, FL"', add
label define metarea_lbl 9000 `"Wheeling, WV/OH"', add
label define metarea_lbl 9040 `"Wichita, KS"', add
label define metarea_lbl 9140 `"Williamsport, PA"', add
label define metarea_lbl 9160 `"Wilmington, DE/NJ/MD"', add
label define metarea_lbl 9200 `"Wilmington, NC"', add
label define metarea_lbl 9240 `"Worcester, MA"', add
label define metarea_lbl 9260 `"Yakima, WA"', add
label define metarea_lbl 9270 `"Yolo, CA"', add
label define metarea_lbl 9280 `"York, PA"', add
label define metarea_lbl 9281 `"York-Hanover, PA"', add
label define metarea_lbl 9320 `"Youngstown-Warren, OH/PA"', add
label define metarea_lbl 9321 `"Youngstown-Warren-Boardman, OH"', add
label define metarea_lbl 9340 `"Yuba City, CA"', add
label define metarea_lbl 9360 `"Yuma, AZ"', add
label define metarea_lbl 9997 `"Other metropolitan areas, unidentified"', add
label define metarea_lbl 9998 `"NIU, household not in a metropolitan area"', add
label define metarea_lbl 9999 `"Missing data"', add
label values metarea metarea_lbl

label define month_lbl 01 `"January"'
label define month_lbl 02 `"February"', add
label define month_lbl 03 `"March"', add
label define month_lbl 04 `"April"', add
label define month_lbl 05 `"May"', add
label define month_lbl 06 `"June"', add
label define month_lbl 07 `"July"', add
label define month_lbl 08 `"August"', add
label define month_lbl 09 `"September"', add
label define month_lbl 10 `"October"', add
label define month_lbl 11 `"November"', add
label define month_lbl 12 `"December"', add
label values month month_lbl

label define wtsupp_lbl 0000000000 `"0000000000"'
label values wtsupp wtsupp_lbl

label define age_lbl 00 `"Under 1 year"'
label define age_lbl 01 `"1"', add
label define age_lbl 02 `"2"', add
label define age_lbl 03 `"3"', add
label define age_lbl 04 `"4"', add
label define age_lbl 05 `"5"', add
label define age_lbl 06 `"6"', add
label define age_lbl 07 `"7"', add
label define age_lbl 08 `"8"', add
label define age_lbl 09 `"9"', add
label define age_lbl 10 `"10"', add
label define age_lbl 11 `"11"', add
label define age_lbl 12 `"12"', add
label define age_lbl 13 `"13"', add
label define age_lbl 14 `"14"', add
label define age_lbl 15 `"15"', add
label define age_lbl 16 `"16"', add
label define age_lbl 17 `"17"', add
label define age_lbl 18 `"18"', add
label define age_lbl 19 `"19"', add
label define age_lbl 20 `"20"', add
label define age_lbl 21 `"21"', add
label define age_lbl 22 `"22"', add
label define age_lbl 23 `"23"', add
label define age_lbl 24 `"24"', add
label define age_lbl 25 `"25"', add
label define age_lbl 26 `"26"', add
label define age_lbl 27 `"27"', add
label define age_lbl 28 `"28"', add
label define age_lbl 29 `"29"', add
label define age_lbl 30 `"30"', add
label define age_lbl 31 `"31"', add
label define age_lbl 32 `"32"', add
label define age_lbl 33 `"33"', add
label define age_lbl 34 `"34"', add
label define age_lbl 35 `"35"', add
label define age_lbl 36 `"36"', add
label define age_lbl 37 `"37"', add
label define age_lbl 38 `"38"', add
label define age_lbl 39 `"39"', add
label define age_lbl 40 `"40"', add
label define age_lbl 41 `"41"', add
label define age_lbl 42 `"42"', add
label define age_lbl 43 `"43"', add
label define age_lbl 44 `"44"', add
label define age_lbl 45 `"45"', add
label define age_lbl 46 `"46"', add
label define age_lbl 47 `"47"', add
label define age_lbl 48 `"48"', add
label define age_lbl 49 `"49"', add
label define age_lbl 50 `"50"', add
label define age_lbl 51 `"51"', add
label define age_lbl 52 `"52"', add
label define age_lbl 53 `"53"', add
label define age_lbl 54 `"54"', add
label define age_lbl 55 `"55"', add
label define age_lbl 56 `"56"', add
label define age_lbl 57 `"57"', add
label define age_lbl 58 `"58"', add
label define age_lbl 59 `"59"', add
label define age_lbl 60 `"60"', add
label define age_lbl 61 `"61"', add
label define age_lbl 62 `"62"', add
label define age_lbl 63 `"63"', add
label define age_lbl 64 `"64"', add
label define age_lbl 65 `"65"', add
label define age_lbl 66 `"66"', add
label define age_lbl 67 `"67"', add
label define age_lbl 68 `"68"', add
label define age_lbl 69 `"69"', add
label define age_lbl 70 `"70"', add
label define age_lbl 71 `"71"', add
label define age_lbl 72 `"72"', add
label define age_lbl 73 `"73"', add
label define age_lbl 74 `"74"', add
label define age_lbl 75 `"75"', add
label define age_lbl 76 `"76"', add
label define age_lbl 77 `"77"', add
label define age_lbl 78 `"78"', add
label define age_lbl 79 `"79"', add
label define age_lbl 80 `"80"', add
label define age_lbl 81 `"81"', add
label define age_lbl 82 `"82"', add
label define age_lbl 83 `"83"', add
label define age_lbl 84 `"84"', add
label define age_lbl 85 `"85"', add
label define age_lbl 86 `"86"', add
label define age_lbl 87 `"87"', add
label define age_lbl 88 `"88"', add
label define age_lbl 89 `"89"', add
label define age_lbl 90 `"90 (90+, 1988-2002)"', add
label define age_lbl 91 `"91"', add
label define age_lbl 92 `"92"', add
label define age_lbl 93 `"93"', add
label define age_lbl 94 `"94"', add
label define age_lbl 95 `"95"', add
label define age_lbl 96 `"96"', add
label define age_lbl 97 `"97"', add
label define age_lbl 98 `"98"', add
label define age_lbl 99 `"99+"', add
label values age age_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label define sex_lbl 9 `"NIU"', add
label values sex sex_lbl

label define race_lbl 100 `"White"'
label define race_lbl 200 `"Black/Negro"', add
label define race_lbl 300 `"American Indian/Aleut/Eskimo"', add
label define race_lbl 650 `"Asian or Pacific Islander"', add
label define race_lbl 651 `"Asian only"', add
label define race_lbl 652 `"Hawaiian/Pacific Islander only"', add
label define race_lbl 700 `"Other (single) race, n.e.c."', add
label define race_lbl 801 `"White-Black"', add
label define race_lbl 802 `"White-American Indian"', add
label define race_lbl 803 `"White-Asian"', add
label define race_lbl 804 `"White-Hawaiian/Pacific Islander"', add
label define race_lbl 805 `"Black-American Indian"', add
label define race_lbl 806 `"Black-Asian"', add
label define race_lbl 807 `"Black-Hawaiian/Pacific Islander"', add
label define race_lbl 808 `"American Indian-Asian"', add
label define race_lbl 809 `"Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 810 `"White-Black-American Indian"', add
label define race_lbl 811 `"White-Black-Asian"', add
label define race_lbl 812 `"White-American Indian-Asian"', add
label define race_lbl 813 `"White-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 814 `"White-Black-American Indian-Asian"', add
label define race_lbl 815 `"American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 816 `"White-Black--Hawaiian/Pacific Islander"', add
label define race_lbl 817 `"White-American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 818 `"Black-American Indian-Asian"', add
label define race_lbl 819 `"White-American Indian-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 820 `"Two or three races, unspecified"', add
label define race_lbl 830 `"Four or five races, unspecified"', add
label define race_lbl 999 `"NIU"', add
label values race race_lbl

label define educ_lbl 000 `"NIU or no schooling"'
label define educ_lbl 001 `"NIU"', add
label define educ_lbl 002 `"None or preschool"', add
label define educ_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_lbl 011 `"Grade 1"', add
label define educ_lbl 012 `"Grade 2"', add
label define educ_lbl 013 `"Grade 3"', add
label define educ_lbl 014 `"Grade 4"', add
label define educ_lbl 020 `"Grades 5 or 6"', add
label define educ_lbl 021 `"Grade 5"', add
label define educ_lbl 022 `"Grade 6"', add
label define educ_lbl 030 `"Grades 7 or 8"', add
label define educ_lbl 031 `"Grade 7"', add
label define educ_lbl 032 `"Grade 8"', add
label define educ_lbl 040 `"Grade 9"', add
label define educ_lbl 050 `"Grade 10"', add
label define educ_lbl 060 `"Grade 11"', add
label define educ_lbl 070 `"Grade 12"', add
label define educ_lbl 071 `"12th grade, no diploma"', add
label define educ_lbl 072 `"12th grade, diploma unclear"', add
label define educ_lbl 073 `"High school diploma or equivalent"', add
label define educ_lbl 080 `"1 year of college"', add
label define educ_lbl 081 `"Some college but no degree"', add
label define educ_lbl 090 `"2 years of college"', add
label define educ_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_lbl 092 `"Associate's degree, academic program"', add
label define educ_lbl 100 `"3 years of college"', add
label define educ_lbl 110 `"4 years of college"', add
label define educ_lbl 111 `"Bachelor's degree"', add
label define educ_lbl 120 `"5+ years of college"', add
label define educ_lbl 121 `"5 years of college"', add
label define educ_lbl 122 `"6+ years of college"', add
label define educ_lbl 123 `"Master's degree"', add
label define educ_lbl 124 `"Professional school degree"', add
label define educ_lbl 125 `"Doctorate degree"', add
label define educ_lbl 999 `"Missing/Unknown"', add
label values educ educ_lbl

label define empstat_lbl 00 `"NIU"'
label define empstat_lbl 01 `"Armed Forces"', add
label define empstat_lbl 10 `"At work"', add
label define empstat_lbl 12 `"Has job, not at work last week"', add
label define empstat_lbl 20 `"Unemployed"', add
label define empstat_lbl 21 `"Unemployed, experienced worker"', add
label define empstat_lbl 22 `"Unemployed, new worker"', add
label define empstat_lbl 30 `"Not in labor force"', add
label define empstat_lbl 31 `"NILF, housework"', add
label define empstat_lbl 32 `"NILF, unable to work"', add
label define empstat_lbl 33 `"NILF, school"', add
label define empstat_lbl 34 `"NILF, other"', add
label define empstat_lbl 35 `"NILF, unpaid, lt 15 hours"', add
label values empstat empstat_lbl

label define labforce_lbl 0 `"NIU"'
label define labforce_lbl 1 `"No, not in the labor force"', add
label define labforce_lbl 2 `"Yes, in the labor force"', add
label values labforce labforce_lbl

label define occ_lbl 0000 `"0"'
label define occ_lbl 0001 `"1"', add
label define occ_lbl 0002 `"2"', add
label define occ_lbl 0003 `"3"', add
label define occ_lbl 0004 `"4"', add
label define occ_lbl 0005 `"5"', add
label define occ_lbl 0006 `"6"', add
label define occ_lbl 0007 `"7"', add
label define occ_lbl 0008 `"8"', add
label define occ_lbl 0009 `"9"', add
label define occ_lbl 0010 `"10"', add
label define occ_lbl 0011 `"11"', add
label define occ_lbl 0012 `"12"', add
label define occ_lbl 0013 `"13"', add
label define occ_lbl 0014 `"14"', add
label define occ_lbl 0015 `"15"', add
label define occ_lbl 0016 `"16"', add
label define occ_lbl 0017 `"17"', add
label define occ_lbl 0018 `"18"', add
label define occ_lbl 0019 `"19"', add
label define occ_lbl 0020 `"20"', add
label define occ_lbl 0021 `"21"', add
label define occ_lbl 0022 `"22"', add
label define occ_lbl 0023 `"23"', add
label define occ_lbl 0024 `"24"', add
label define occ_lbl 0025 `"25"', add
label define occ_lbl 0026 `"26"', add
label define occ_lbl 0027 `"27"', add
label define occ_lbl 0028 `"28"', add
label define occ_lbl 0029 `"29"', add
label define occ_lbl 0030 `"30"', add
label define occ_lbl 0031 `"31"', add
label define occ_lbl 0032 `"32"', add
label define occ_lbl 0033 `"33"', add
label define occ_lbl 0034 `"34"', add
label define occ_lbl 0035 `"35"', add
label define occ_lbl 0036 `"36"', add
label define occ_lbl 0037 `"37"', add
label define occ_lbl 0099 `"99"', add
label values occ occ_lbl

label define ind_lbl 0000 `"0"'
label values ind ind_lbl

label define inctot_lbl 00999997 `"00999997"'
label define inctot_lbl 99999997 `"99999997"', add
label define inctot_lbl 99999999 `"99999999"', add
label values inctot inctot_lbl

label define incwage_lbl 9999997 `"9999997"'
label define incwage_lbl 9999999 `"9999999"', add
label values incwage incwage_lbl

label define incbus_lbl 9999997 `"9999997"'
label define incbus_lbl 9999999 `"9999999"', add
label values incbus incbus_lbl

label define incfarm_lbl 9999997 `"9999997"'
label define incfarm_lbl 9999999 `"9999999"', add
label values incfarm incfarm_lbl

