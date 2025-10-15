## Prijava ranjivosti

Iznimno smo zahvalni sigurnosnim istraživačima i korisnicima koji Hasura zajednici prijavljuju ranjivosni. Sve prijave su temeljito obrađene od strane grupe volontera iz zajednice, te Hasura tima.

Kako biste prijavili sigurnosni problem, molimo pošaljite na e-mail na adresu [build@hasura.io](mailto:build@hasura.io) sa naznačenim detaljima, prilažući sve potrebne informacije.

### Kada trebate prijaviti ranjivost?

- Mislite da ste otkrili potencijalnu sigurnosnu ranjivost u Hasura GraphQL Engine-u ili povezanim komponentama.
- Niste sigurni kako ranjivost utiče na Hasura GraphQL Engine.
- Mislite da ste otkrili ranjivost u projektu od kojeg zavisi Hasura GraphQL Engine (npr. Heroku, Docker, itd.).
- Želite prijeviti bilo koji drugi sigurnosni rizik koji bi potencijalno mogao naškoditi korisnicima Hasura GraphQL Engine-a.

### Kada NE bi trebali prijaviti ranjivost?

- Trebate pomoć pri podešavanju Hasura GraphQL Engine sigurnosnih komponenata.
- Trebate pomoć pri dodavanju ažuriranja vezanih za sigurnost.
- Vaš problem nije vezan za sigurnost.

## Odgovor na sigurnosnu ranjivost

Svaka prijava je verificirana i analizirana od strane održavatelja projekta i tima za sigurnost u roku od 3 radna dana. 

Podnositelj prijave će biti obaviješten kroz svaku fazu analize problema i njegovog rješavanja (razriješi -> popravi -> distribuiraj).

## Vrijeme javnog objavljivanja

Datum javnog objavljivanja pregovara Hasura tim za sigurnost proizvoda i podnosilac prijave o softverskoj grešci. Preferiramo da upotpunosti otkrijemo grešku što je prije moguće onda kada je dostupno ublažavanje korisnika. Razumno je odgoditi objavu kada greška ili popravak još uvijek nisu u potpunosti jasni, rješenje nije dobro testirano ili zbog koordinacije dobavljača. Vremenski okvir objave se može kretati od trenutnog (posebno ako je već poznat javnosti) do nekoliko sedmica. Očekujemo da vremenski okvir od prijave do javne objave uglavnom traje do 7 dana. Održavatelji Hasura GraphQL Engine-a i tim za sigurnost su ti koji će na kraju odrediti konačan datum objave.


(Određeni dijelovi su inspirisani i adaptirani iz [https://github.com/kubernetes/website/blob/master/content/en/docs/reference/issues-security/security.md](https://github.com/kubernetes/website/blob/master/content/en/docs/reference/issues-security/security.md)).
