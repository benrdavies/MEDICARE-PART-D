# MEDICARE-PART-D
This is the code I used to analyze Medicare Part D Billing Data; the file is 23 million lines so I used the Data.table R package to load and perform the analysis. 
It takes about 15 mins to to run on my laptop; I'm just learning R so I'm sure people can find ways to get it running faster.

The code compares drug prices in the USA and Australia; it calculate the average per day Medicare Part D price then compares it to the Australian PBS prices. 
The July4....PBS2013data.csv file is the comparison file of with Australian Drug prices; as it stands the code runs the top 50 drugs and searchs the PBS file for the highest per unit price.
but could also analyze all of them ~2700 of them.

I then use the pyramid package to visualize the difference in prices between countries.

The Nexium is a Total Rip Off file lists all the doctors in America who proscribe Nexium vs. its generic version Ozemprazole at more than 95% of the time. It lists them by name, this was done because Nexium costs 17x more than its generic version so it is a travesty they are wasting so much money.

The Medicare Part D dataset is incrediably rich, the last time data was released for doctor billings a lot of fraud was revealed. It is clear this systems is very wastefull, Happy hunting!!!
