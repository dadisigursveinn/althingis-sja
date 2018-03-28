import untangle
import csv

# Scraper that fetches information about members of congress and the parties they were with
# writes the information to a csv file

logging = True
writeResults = True

outputFile = '../data/members_details.csv'
results = []

ids = [] # holds the ids of all members of congress
# we start by finding all id's used
obj = untangle.parse('http://www.althingi.is/altext/xml/thingmenn/')

if(logging):
    print("Starting to fetch id's of members")

congressmen = 0
for thingmadur in obj.þingmannalisti.children:
    ids.append(thingmadur['id'])
    congressmen += 1

if(logging):
    print("Finished fetching id's")
    print("Fetched id's for " + str(congressmen) + " members of congress.")

# we now fetch details for each member of congress
if (logging):
    print("Fetching details about members of congress")
for id in ids:
    obj = untangle.parse('http://www.althingi.is/altext/xml/thingmenn/thingmadur/thingseta/?nr=' + str(id))
    #print(obj.þingmaður.nafn.cdata)
    for congress in obj.þingmaður.þingsetur.children:
        results.append([
            obj.þingmaður['id'],
            obj.þingmaður.nafn.cdata,
            congress.þing.cdata,
            congress.tegund.cdata,
            congress.þingflokkur['id'],
            congress.kjördæmi['id'],
            congress.tímabil.inn.cdata,
            congress.tímabil.út.cdata
            ])

if (logging):
    print("Finished fetching details about members of congress")

if(writeResults):
    print('Writing to ' + outputFile)
    summaryCSV = open(outputFile, 'w')
    with summaryCSV:
        writer = csv.writer(summaryCSV)
        writer.writerows(results)
    print('Finished writing data to ' + outputFile)