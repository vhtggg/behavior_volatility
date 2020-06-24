select
    date,
    sum(balance) as balance
from portfolio 
where 
    currency = "VND" and
    client_type = "ABC" and
    ownership_type = "SG"
group by 
    currency,
    client_type,
    ownership_type,
    date
order by 
    currency,
    client_type,
    ownership_type,
    date
;
