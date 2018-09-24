(* Solution to Exercise 4.23 using Set and Map libraries *)
type Sex =  Male | Female
type Name = {first: string; last: string}
type Phone = {prefix: int; number: int}
type Year = int
type Interests = Set<string>
type RecordID = int

type DatingRecord = {name: Name;
                     phone: Phone;
                     sex: Sex;
                     birthYear: Year;
                     interests: Interests}

type DatingDatabase = Map<RecordID, DatingRecord>

let recordsMatch datingRecord1 datingRecord2 = 
    datingRecord1.sex <> datingRecord2.sex 
    && (Set.intersect datingRecord1.interests datingRecord2.interests) <> Set.empty
    && System.Math.Abs (datingRecord1.birthYear - datingRecord2.birthYear) <= 10

let findMatches datingRecord datingDatabase =
    Map.fold (fun matches id entry -> if recordsMatch entry datingRecord 
                                        then Map.add id entry matches 
                                        else matches)
             Map.empty
             datingDatabase