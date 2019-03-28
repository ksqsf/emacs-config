#!/bin/bash
printf "Generated on $(env LC_ALL=C date)\n" > external
printf 'Extract the list of packages by `grep ^elpa- external`\n\n' >> external
aptitude search '^elpa-~i' -F %p >> external
