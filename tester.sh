urpmi postgresql-server
pg_ctl init -D db
pg_ctl -D db -l logdb start
createuser test
createdb -O test perservant
psql perservant
\password test
   test
grant all privileges on database perservant to test


delete from refs_new;
alter table refs_new auto_increment=1351;


===============================
╰─$ curl -vvv "localhost:3000/auth/signin?login=admin&password=123456"

replied by:
{"token":"b427e9c8-8c97-4851-b657-c4500ba78031"}

then calling with:
curl -vvv -H "Authorization: 4c364cfe-5db0-47e5-a4fd-c58846bb06a0" "localhost:3000/test"

signout:
╰─$ curl -vvv -X POST -H "Authorization: 4c364cfe-5db0-47e5-a4fd-c58846bb06a0" "localhost:3000/auth/signout"

