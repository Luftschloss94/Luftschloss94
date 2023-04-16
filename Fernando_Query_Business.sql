#Make sure to run the following query before reviewing the rest of the file 
USE bos_fmban_sql_analysis;

#Insight: List of queries to review the portfolio for 365 Whole Foods Market Brand 
# This Query runs as table 1 to clean the categories 
USE bos_fmban_sql_analysis;
SELECT 
    CASE
        WHEN CATEGORY = 'WINE BEER SPIRITS' THEN 'WINE BEER SPIRITS'
        WHEN CATEGORY = 'DAIRY AND EGGS' THEN 'DAIRY AND EGGS'
        WHEN CATEGORY = 'DAIRY AND BUTTER' THEN 'DAIRY AND BUTTER'
        WHEN CATEGORY = 'MEAT' THEN 'MEAT'
        WHEN CATEGORY = 'FROZEN FOOD' THEN 'FROZEN FOODS'
        WHEN CATEGORY = 'FROZEN FOODS' THEN 'FROZEN FOODS'
        WHEN CATEGORY = 'SUPPLEMENTS' THEN 'SUPPLEMENTS'
        WHEN CATEGORY = 'SNACKS CHIPS SALSAS DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
        WHEN CATEGORY = 'SNACKS CHIPS SALSAS AND DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
        WHEN CATEGORY = 'SEAFOOD' THEN 'SEAFOOD'
        WHEN CATEGORY = 'PRODUCE' THEN 'PRODUCE'
        WHEN CATEGORY = 'PREPARED FOODS' THEN 'PREPARED FOODS'
        WHEN CATEGORY = 'PANTRY ESSENTIALS' THEN 'PANTRY ESSENTIALS'
        WHEN CATEGORY = 'MEAT' THEN 'MEAT'
        WHEN CATEGORY = 'LIFESTYLE' THEN 'LIFESTYLE'
        WHEN CATEGORY = 'FLORAL' THEN 'FLORAL'
        WHEN CATEGORY = 'DESSERTS' THEN 'DESSERTS'
        WHEN CATEGORY = 'BREAD ROLLS AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
        WHEN CATEGORY = 'BREAD ROLL AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
        WHEN CATEGORY = 'BODY CARE' THEN 'BODY CARE'
        WHEN CATEGORY = 'BEVERAGES' THEN 'BEVERAGES'
        WHEN CATEGORY = 'BEAUTY' THEN 'BEAUTY'
    END AS category_clean
FROM
    bfmban_data
GROUP BY Category , category_clean
;
 /* Query to filter Whole Foods Yes or No */ 
SELECT 
    BRAND,
    CASE
        WHEN BRAND LIKE '%WHOLE FOODS MARKET%' THEN 'whole_food'
        WHEN BRAND LIKE '%365%' THEN 'whole_food'
        ELSE 'other_brand'
    END AS is_whole_food_brand
FROM
    bfmban_data
;
 /*Query to Run the percentage of Category/Total */
WITH tabla1 AS(
SELECT 
CATEGORY, COUNT(ID) as Total_Count
FROM
    bos_fmban_sql_analysis.bfmban_data
    GROUP BY CATEGORY)

SELECT 
CATEGORY,
CONCAT(ROUND((COUNT(Category)/total_count),2),'%') AS percentage
FROM tabla1
GROUP BY CATEGORY
;

/* Insight Query, the Query uses 2 WITH tables for Category and the Total Count.
This queries then are joined through the category and the operation is calculated so we can know the Store own id count, total count, and percentage of store owned articles per category  */

#Query of Category defines and clean each category
WITH category AS(
SELECT 
CASE WHEN CATEGORY =  'WINE BEER SPIRITS'  THEN  'WINE BEER SPIRITS'
WHEN CATEGORY = 'DAIRY AND EGGS' THEN 'DAIRY AND EGGS'
WHEN CATEGORY = 'DAIRY AND BUTTER' THEN 'DAIRY AND BUTTER'
WHEN CATEGORY = 'MEAT' THEN 'MEAT'
WHEN CATEGORY = 'FROZEN FOOD' THEN 'FROZEN FOODS'
WHEN CATEGORY = 'FROZEN FOODS' THEN 'FROZEN FOODS'
WHEN CATEGORY = 'SUPPLEMENTS' THEN 'SUPPLEMENTS'
WHEN CATEGORY = 'SNACKS CHIPS SALSAS DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
WHEN CATEGORY = 'SNACKS CHIPS SALSAS AND DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
WHEN CATEGORY = 'SEAFOOD' THEN 'SEAFOOD' 
WHEN CATEGORY = 'PRODUCE' THEN 'PRODUCE'
WHEN CATEGORY = 'PREPARED FOODS' THEN 'PREPARED FOODS'
WHEN CATEGORY = 'PANTRY ESSENTIALS' THEN 'PANTRY ESSENTIALS'
WHEN CATEGORY = 'MEAT' THEN 'MEAT'
WHEN CATEGORY = 'LIFESTYLE' THEN 'LIFESTYLE'
WHEN CATEGORY = 'FLORAL' THEN 'FLORAL'
WHEN CATEGORY = 'DESSERTS' THEN 'DESSERTS'
WHEN CATEGORY ='BREAD ROLLS AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
WHEN CATEGORY = 'BREAD ROLL AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
WHEN CATEGORY = 'BODY CARE' THEN 'BODY CARE'
WHEN CATEGORY = 'BEVERAGES' THEN 'BEVERAGES'
WHEN CATEGORY = 'BEAUTY' THEN 'BEAUTY'
END AS category_clean
 FROM bfmban_data 
 GROUP BY category_clean ),
 
 #Query of total count takes on the previously cleaned category information and add the total count so we can start calculating store brand/total brands 
total_count AS(
SELECT 
  CASE WHEN CATEGORY =  'WINE BEER SPIRITS'  THEN  'WINE BEER SPIRITS'
WHEN CATEGORY = 'DAIRY AND EGGS' THEN 'DAIRY AND EGGS'
WHEN CATEGORY = 'DAIRY AND BUTTER' THEN 'DAIRY AND BUTTER'
WHEN CATEGORY = 'MEAT' THEN 'MEAT'
WHEN CATEGORY = 'FROZEN FOOD' THEN 'FROZEN FOODS'
WHEN CATEGORY = 'FROZEN FOODS' THEN 'FROZEN FOODS'
WHEN CATEGORY = 'SUPPLEMENTS' THEN 'SUPPLEMENTS'
WHEN CATEGORY = 'SNACKS CHIPS SALSAS DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
WHEN CATEGORY = 'SNACKS CHIPS SALSAS AND DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
WHEN CATEGORY = 'SEAFOOD' THEN 'SEAFOOD' 
WHEN CATEGORY = 'PRODUCE' THEN 'PRODUCE'
WHEN CATEGORY = 'PREPARED FOODS' THEN 'PREPARED FOODS'
WHEN CATEGORY = 'PANTRY ESSENTIALS' THEN 'PANTRY ESSENTIALS'
WHEN CATEGORY = 'MEAT' THEN 'MEAT'
WHEN CATEGORY = 'LIFESTYLE' THEN 'LIFESTYLE'
WHEN CATEGORY = 'FLORAL' THEN 'FLORAL'
WHEN CATEGORY = 'DESSERTS' THEN 'DESSERTS'
WHEN CATEGORY ='BREAD ROLLS AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
WHEN CATEGORY = 'BREAD ROLL AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
WHEN CATEGORY = 'BODY CARE' THEN 'BODY CARE'
WHEN CATEGORY = 'BEVERAGES' THEN 'BEVERAGES'
WHEN CATEGORY = 'BEAUTY' THEN 'BEAUTY'
END AS category_clean,
 COUNT(ID) as Total_Count
FROM
    bos_fmban_sql_analysis.bfmban_data
     GROUP BY category_clean
     )

#Final Query eliminates duplicates and creates inner joins with the clean category, additionally makes the calculation to determine home brand percentage per row 
SELECT DISTINCT
    `c`.`category_clean`,
    whole.is_whole_food_brand,
    COUNT(whole.is_whole_food_brand) AS home_brand_count,
    tc.Total_Count,
    CONCAT(ROUND((COUNT(whole.is_whole_food_brand) / tc.total_count * 100),
                    2),
            '%') AS home_brand_percentage
FROM
    (SELECT 
        CASE
                WHEN CATEGORY = 'WINE BEER SPIRITS' THEN 'WINE BEER SPIRITS'
                WHEN CATEGORY = 'DAIRY AND EGGS' THEN 'DAIRY AND EGGS'
                WHEN CATEGORY = 'DAIRY AND BUTTER' THEN 'DAIRY AND BUTTER'
                WHEN CATEGORY = 'MEAT' THEN 'MEAT'
                WHEN CATEGORY = 'FROZEN FOOD' THEN 'FROZEN FOODS'
                WHEN CATEGORY = 'FROZEN FOODS' THEN 'FROZEN FOODS'
                WHEN CATEGORY = 'SUPPLEMENTS' THEN 'SUPPLEMENTS'
                WHEN CATEGORY = 'SNACKS CHIPS SALSAS DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
                WHEN CATEGORY = 'SNACKS CHIPS SALSAS AND DIPS' THEN 'SNACKS CHIPS SALSAS AND DIPS'
                WHEN CATEGORY = 'SEAFOOD' THEN 'SEAFOOD'
                WHEN CATEGORY = 'PRODUCE' THEN 'PRODUCE'
                WHEN CATEGORY = 'PREPARED FOODS' THEN 'PREPARED FOODS'
                WHEN CATEGORY = 'PANTRY ESSENTIALS' THEN 'PANTRY ESSENTIALS'
                WHEN CATEGORY = 'MEAT' THEN 'MEAT'
                WHEN CATEGORY = 'LIFESTYLE' THEN 'LIFESTYLE'
                WHEN CATEGORY = 'FLORAL' THEN 'FLORAL'
                WHEN CATEGORY = 'DESSERTS' THEN 'DESSERTS'
                WHEN CATEGORY = 'BREAD ROLLS AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
                WHEN CATEGORY = 'BREAD ROLL AND BAKERY' THEN 'BREAD ROLLS AND BAKERY'
                WHEN CATEGORY = 'BODY CARE' THEN 'BODY CARE'
                WHEN CATEGORY = 'BEVERAGES' THEN 'BEVERAGES'
                WHEN CATEGORY = 'BEAUTY' THEN 'BEAUTY'
            END AS category_clean,
            BRAND,
            CASE
                WHEN BRAND LIKE '%WHOLE FOODS MARKET%' THEN 'whole_food'
                WHEN BRAND LIKE '%365%' THEN 'whole_food'
                ELSE 'other_brand'
            END AS is_whole_food_brand
    FROM
        bfmban_data) AS whole
        INNER JOIN
    CATEGORY AS c ON c.category_clean = whole.category_clean
        INNER JOIN
    total_count AS tc ON tc.category_clean = whole.category_clean
GROUP BY c.category_clean , whole.is_whole_food_brand
HAVING is_whole_food_brand = 'whole_food'
ORDER BY home_brand_percentage DESC
;
 
 #Insight: List of Queries to review the average price of Store Owned products vs Other Brand products 
 /*Simplified Price Query
 This query helps us to understand the average price depending if the product is store brand or not*/
SELECT 
    CATEGORY,
    CASE
        WHEN BRAND LIKE '%WHOLE FOODS MARKET%' THEN 'whole_food'
        WHEN BRAND LIKE '%365%' THEN 'whole_food'
        ELSE 'other_brand'
    END AS is_whole_food_brand,
    ROUND(AVG(REGULAR_PRICE), 2) AS avg_price
FROM
    bfmban_data
GROUP BY CATEGORY , is_whole_food_brand
;

/* Calculate the Price*/ 
SELECT 
    CATEGORY,
    CASE
        WHEN is_whole_food_brand LIKE '%whole_food%' THEN avg_price
    END AS avg_price_store_brand,
    CASE
        WHEN is_whole_food_brand LIKE '%other_brand%' THEN avg_price
    END AS avg_price_other_brand
FROM
    (SELECT 
        CATEGORY,
            CASE
                WHEN BRAND LIKE '%WHOLE FOODS MARKET%' THEN 'whole_food'
                WHEN BRAND LIKE '%365%' THEN 'whole_food'
                ELSE 'other_brand'
            END AS is_whole_food_brand,
            ROUND(AVG(REGULAR_PRICE), 2) AS avg_price
    FROM
        bfmban_data
    GROUP BY CATEGORY , is_whole_food_brand) AS pricing
GROUP BY CATEGORY , avg_price_store_brand , avg_price_other_brand
;

#Table 1 for Insight on average store brand price 
WITH brand_price AS(SELECT 
CATEGORY, 
CASE WHEN is_whole_food_brand LIKE '%whole_food%' THEN avg_price  END AS avg_price_store_brand
FROM 
(SELECT 
CATEGORY, 
CASE 
WHEN BRAND LIKE '%WHOLE FOODS MARKET%' THEN 'whole_food' 
WHEN BRAND LIKE '%365%'  THEN 'whole_food'
ELSE 'other_brand'
END AS is_whole_food_brand,
ROUND(AVG(REGULAR_PRICE),2) AS avg_price
FROM bfmban_data
GROUP BY CATEGORY, is_whole_food_brand
) AS pricing
GROUP BY CATEGORY, avg_price_store_brand
HAVING avg_price_store_brand IS NOT NULL),

#Table 2 for insight on other brand average store price 
 other_price AS(SELECT 
CATEGORY, 
CASE WHEN is_whole_food_brand LIKE '%other_brand%'  THEN avg_price END AS avg_price_other_brand
FROM 
(SELECT 
CATEGORY, 
CASE 
WHEN BRAND LIKE '%WHOLE FOODS MARKET%' THEN 'whole_food' 
WHEN BRAND LIKE '%365%'  THEN 'whole_food'
ELSE 'other_brand'
END AS is_whole_food_brand,
ROUND(AVG(REGULAR_PRICE),2) AS avg_price
FROM bfmban_data
GROUP BY CATEGORY, is_whole_food_brand
) AS pricing
GROUP BY CATEGORY, avg_price_other_brand
HAVING avg_price_other_brand IS NOT NULL)

/*On the final select we arrange average store price vs average non-store price and compare if Whole Foods it's actually using a price differentiation strategy 
to establish a competitive advantage vs other brands */
SELECT 
    bp.CATEGORY,
    bp.avg_price_store_brand,
    op.avg_price_other_brand,
    CASE
        WHEN avg_price_store_brand < avg_price_other_brand THEN 'price differentiation'
        ELSE 'no price differentiation'
    END AS pricing_strategy
FROM
    brand_price AS bp
        INNER JOIN
    other_price AS op ON bp.CATEGORY = op.CATEGORY
GROUP BY bp.CATEGORY , bp.avg_price_store_brand , op.avg_price_other_brand
;
