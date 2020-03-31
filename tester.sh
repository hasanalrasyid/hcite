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

CREATE DATABASE `example` /*!40100 DEFAULT CHARACTER SET utf8mb4 */;

===============================
╰─$ curl -vvv "localhost:3000/auth/signin?login=admin&password=123456"

replied by:
{"token":"b427e9c8-8c97-4851-b657-c4500ba78031"}

then calling with:
curl -vvv -H "Authorization: 4c364cfe-5db0-47e5-a4fd-c58846bb06a0" "localhost:3000/test"

signout:
╰─$ curl -vvv -X POST -H "Authorization: 4c364cfe-5db0-47e5-a4fd-c58846bb06a0" "localhost:3000/auth/signout"

╰─$ curl -X PUT -H "Authorization:5ca39879-2eb1-4faa-80a0-9e90f777755b" localhost:3000/api/record/66
╰─$ curl -X PUT -H "Authorization:5ca39879-2eb1-4faa-80a0-9e90f777755b" localhost:3000/api/record/66/year/12345

╰─$ curl -X PUT -H "Authorization: $TOKEN" -F bib=@sample/fileSample.bib localhost:3000/api/record

curl -vvv --header "Content-Type: application/json" --request GET --data '["q","electrode"]'  http://localhost:3000/api/record/list/keywords/1

