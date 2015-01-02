-- This module contains a list of all English words, as adapted from
-- http://www-01.sil.org/linguistics/wordlists/english/
--
-- To use, say `import Words` after your module header line and then
-- access the list of words named `allWords`.

module Words where

scrabbleValue :: Char -> Int
scrabbleValue 'a' = 1
scrabbleValue 'b' = 3
scrabbleValue 'c' = 3
scrabbleValue 'd' = 2
scrabbleValue 'e' = 1
scrabbleValue 'f' = 4
scrabbleValue 'g' = 2
scrabbleValue 'h' = 4
scrabbleValue 'i' = 1
scrabbleValue 'j' = 8
scrabbleValue 'k' = 5
scrabbleValue 'l' = 1
scrabbleValue 'm' = 3
scrabbleValue 'n' = 1
scrabbleValue 'o' = 1
scrabbleValue 'p' = 3
scrabbleValue 'q' = 10
scrabbleValue 'r' = 1
scrabbleValue 's' = 1
scrabbleValue 't' = 1
scrabbleValue 'u' = 1
scrabbleValue 'v' = 4
scrabbleValue 'w' = 4
scrabbleValue 'x' = 8
scrabbleValue 'y' = 4
scrabbleValue 'z' = 10
scrabbleValue _ = minBound  -- shouldn't happen

allWords :: [String]
allWords = lines "aa\naah\naahed\naahing\naahs\naal\naalii\naaliis\naals\naardvark\naardvarks\naardwolf\naardwolves\naargh\naarrgh\naarrghh\naas\naasvogel\naasvogels\nab\naba\nabaca\nabacas\nabaci\naback\nabacterial\nabacus\nabacuses\nabaft\nabaka\nabakas\nabalone\nabalones\nabamp\nabampere\nabamperes\nabamps\nabandon\nabandoned\nabandoner\nabandoners\nabandoning\nabandonment\nabandonments\nabandons\nabapical\nabas\nabase\nabased\nabasedly\nabasement\nabasements\nabaser\nabasers\nabases\nabash\nabashed\nabashedly\nabashes\nabashing\nabashment\nabashments\nabasia\nabasias\nabasing\nabatable\nabate\nabated\nabatement\nabatements\nabater\nabaters\nabates\nabating\nabatis\nabatises\nabator\nabators\nabattis\nabattises\nabattoir\nabattoirs\nabaxial\nabaxile\nabaya\nabayas\nabba\nabbacies\nabbacy\nabbas\nabbatial\nabbe\nabbes\nabbess\nabbesses\nabbey\nabbeys\nabbot\nabbotcies\nabbotcy\nabbots\nabbotship\nabbotships\nabbreviate\nabbreviated\nabbreviates\nabbreviating\nabbreviation\nabbreviations\nabbreviator\nabbreviators\nabcoulomb\nabcoulombs\nabdicable\nabdicate\nabdicated\nabdicates\nabdicating\nabdication\nabdications\nabdicator\nabdicators\nabdomen\nabdomens\nabdomina\nabdominal\nabdominally\nabdominals\nabduce\nabduced\nabducens\nabducent\nabducentes\nabduces\nabducing\nabduct\nabducted\nabductee\nabductees\nabducting\nabduction\nabductions\nabductor\nabductores\nabductors\nabducts\nabeam\nabecedarian\nabecedarians\nabed\nabegging\nabele\nabeles\nabelia\nabelian\nabelias\nabelmosk\nabelmosks\naberrance\naberrances\naberrancies\naberrancy\naberrant\naberrantly\naberrants\naberrated\naberration\naberrational\naberrations\nabet\nabetment\nabetments\nabets\nabettal\nabettals\nabetted\nabetter\nabetters\nabetting\nabettor\nabettors\nabeyance\nabeyances\nabeyancies\nabeyancy\nabeyant\nabfarad\nabfarads\nabhenries\nabhenry\nabhenrys\nabhor\nabhorred\nabhorrence\nabhorrences\nabhorrent\nabhorrently\nabhorrer\nabhorrers\nabhorring\nabhors\nabidance\nabidances\nabide\nabided\nabider\nabiders\nabides\nabiding\nabidingly\nabigail\nabigails\nabilities\nability\nabiogeneses\nabiogenesis\nabiogenic\nabiogenically\nabiogenist\nabiogenists\nabiological\nabioses\nabiosis\nabiotic\nabiotically\nabject\nabjection\nabjections\nabjectly\nabjectness\nabjectnesses\nabjuration\nabjurations\nabjure\nabjured\nabjurer\nabjurers\nabjures\nabjuring\nablate\nablated\nablates\nablating\nablation\nablations\nablative\nablatively\nablatives\nablator\nablators\nablaut\nablauts\nablaze\nable\nabled\nablegate\nablegates\nableism\nableisms\nableist\nableists\nabler\nables\nablest\nablings\nablins\nabloom\nabluent\nabluents\nablush\nabluted\nablution\nablutionary\nablutions\nably\nabmho\nabmhos\nabnegate\nabnegated\nabnegates\nabnegating\nabnegation\nabnegations\nabnegator\nabnegators\nabnormal\nabnormalities\nabnormality\nabnormally\nabnormals\nabnormities\nabnormity\nabo\naboard\nabode\naboded\nabodes\naboding\nabohm\nabohms\naboideau\naboideaus\naboideaux\naboil\naboiteau\naboiteaus\naboiteaux\nabolish\nabolishable\nabolished\nabolisher\nabolishers\nabolishes\nabolishing\nabolishment\nabolishments\nabolition\nabolitionary\nabolitionism\nabolitionisms\nabolitionist\nabolitionists\nabolitions\nabolla\nabollae\naboma\nabomas\nabomasa\nabomasal\nabomasi\nabomasum\nabomasus\nabominable\nabominably\nabominate\nabominated\nabominates\nabominating\nabomination\nabominations\nabominator\nabominators\naboon\naboral\naborally\naboriginal\naboriginally\naboriginals\naborigine\naborigines\naborning\nabort\naborted\naborter\naborters\nabortifacient\nabortifacients\naborting\nabortion\nabortionist\nabortionists\nabortions\nabortive\nabortively\nabortiveness\nabortivenesses\naborts\nabortus\nabortuses\nabos\nabought\naboulia\naboulias\naboulic\nabound\nabounded\nabounding\nabounds\nabout\nabove\naboveboard\naboveground\naboves\nabracadabra\nabracadabras\nabrachia\nabrachias\ncut\n"
