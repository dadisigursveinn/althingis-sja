import untangle
import csv

# Scraper that fetches information about members of congress
# id, name, dob,  initials, congress
# writes the information to a csv file

logging = True
writeResults = True

outputFile = 'members_by_congress.csv'
results = [["id", "nafn", "fæðingardagur", "skammstöfun", "þing"]] #schema for csv file
for congress in range(1, 149):
    obj = untangle.parse('http://www.althingi.is/altext/xml/thingmenn/?lthing=' + str(congress))
    if(logging):
        print("Starting to fetch data for congress " + str(congress))
    congressmen = 0
    failed = 0
    for thingmadur in obj.þingmannalisti.children:
        results.append([thingmadur['id'], thingmadur.nafn.cdata, thingmadur.fæðingardagur.cdata, thingmadur.skammstöfun.cdata, str(congress)])
        congressmen += 1
    if(logging):
        print("Finished fetching data for congress " + str(congress))
        print("Fetched " + str(congressmen) + " members of congress.")
if(writeResults):
    print('Writing to ' + outputFile)
    summaryCSV = open(outputFile, 'w')
    with summaryCSV:
        writer = csv.writer(summaryCSV)
        writer.writerows(results)
    print('Finished writing data to ' + outputFile)