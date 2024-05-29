#!/bin/bash

# 1st param - Data source
# 2nd param - Name of the csv file
# 3rd param - Year filter

# Example
# ./scrap_data_V2.sh https://cveawg.mitre.org/api/cve/ results.csv 2015-2018

csv_filename=$2
data_source="https://cveawg.mitre.org/api/cve/"


columns=(
	"cveId"
	"assignerShortName"
	"dateReserved"
	"datePublished"
	"dateUpdated"
	"\"version\""
	"baseSeverity"
	"baseScore"
	"vectorString"
)

generate_cve_list(){
	rm cve_ids.temp
	if [[ "$1" == *-* ]]; then
	    from=$(echo "$1" | cut -d "-" -f1)
	    to=$(echo "$1" | cut -d "-" -f2)
	    echo "[+] From $from"
	    echo "[+] To $to"
	    for i in $(seq $from $to); do
	    	cat number_cve.txt | grep "CVE-$i" >> cve_ids.temp
	    done
	else
		echo "[+] Generating for all years. (1999-2024)"
	    cat number_cve.txt | grep "CVE-$1" > cve_ids.temp
	fi
}

initialize_csv_file(){
	headerRow=""
	for column in "${columns[@]}"; do
		headerRow+="$column,"
	done
	headerRow="${headerRow%?}"
	echo $headerRow
	echo "$headerRow">"$csv_filename"
}

fill_the_data(){
	for cve_id in $(cat cve_ids.temp); do
		echo "[+] $cve_id"
		row_buffer=""
		res=$(curl -s "${data_source}${cve_id}")
		for column in "${columns[@]}"; do
			column_value=$(echo $res | jq | grep -m 1 "$column" | awk '{print $2}' | sed 's/["|,]//g')
			if [[ $column == "cveId" ]]; then
				column_value=$(echo $column_value | sed 's/CVE-//')
			fi
		    row_buffer+="$column_value,"
		done
		row_buffer="${row_buffer%?}"
		echo $row_buffer >> "$csv_filename"
	done
}

clean_the_temp(){
	rm cve_ids.temp
}

generate_cve_list $3
initialize_csv_file
fill_the_data
clean_the_temp