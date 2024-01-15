open Lwt
open Cohttp
open Cohttp_lwt_unix

type bike = Available of int | Maintenance of int

let get_access_token =
  print_string "Fetching access token... ";
  Out_channel.flush Out_channel.stdout;
  let access_token_rq =
    let uri =
      "https://api.cyclocity.fr/auth/environments/PRD/client_tokens"
      |> Uri.of_string
    in
    let body =
      {|{
       "code": "vls.web.ljubljana:PRD",
       "key": "bba78ac7491b6160192007ef22cba4d1e326cf86b68b965bc28f9fb691af6a25"
       }|}
      |> Cohttp_lwt.Body.of_string
    in
    let headers = Header.init_with "Content-Type" "application/json"
    in
    Client.post ~body ~headers uri >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string
  in
  let json = Lwt_main.run access_token_rq |> Yojson.Basic.from_string in
  print_endline "Done!";
  let open Yojson.Basic.Util in
  json |> member "accessToken" |> to_string

let get_stations tkn =
  print_string "Fetching station data... ";
  Out_channel.flush Out_channel.stdout;
  let stations_rq =
    let uri =
      "https://api.jcdecaux.com/vls/v3/stations?apiKey=frifk0jbxfefqqniqez09tw4jvk37wyf823b5j1i&contract=ljubljana"
      |> Uri.of_string
    in
    let headers = Header.init_with "Authorization" ("Taknv1 " ^ tkn) in
    Client.get ~headers uri >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string
  in
  let open Yojson.Basic in 
  let json = Lwt_main.run stations_rq |> from_string in
  print_endline "Done!";
  match json with
  | `List ls ->
     let map_fun el =
       let station_number = el |> Util.member "number" |> Util.to_int in
       let name           = el |> Util.member "name" |> Util.to_string in
       (station_number, name)
     in
     List.map map_fun ls
  | _ -> failwith "Expected list"

let get_bikes tkn =
  print_string ("Fetching bike data... ");
  Out_channel.flush Out_channel.stdout;
  let station_bikes_rq =
    let uri =
      "https://api.cyclocity.fr/contracts/ljubljana/bikes"
      |> Uri.of_string
    in
    let headers =
      Header.of_list
        [ ("Authorization", "Taknv1 " ^ tkn)
        ; ("Accept", "application/vnd.bikes.v4+json")
        ]
    in
    Client.get ~headers uri >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string
  in
  let open Yojson.Basic in 
  let json = Lwt_main.run station_bikes_rq |> from_string in
  print_endline "Done!";
  match json with
  | `List ls ->
     let bikes = Hashtbl.create 1000 in
     let get_bike_data el =
       let bike_number    = el |> Util.member "number" |> Util.to_int in
       let station_number = el |> Util.member "stationNumber" |> Util.to_int in
       let status         = el |> Util.member "status" |> Util.to_string in
       match status with
       | "AVAILABLE"   -> Hashtbl.add bikes station_number (Available bike_number)
       | "MAINTENANCE" -> Hashtbl.add bikes station_number (Maintenance bike_number)
       | str -> failwith str
     in
     List.iter get_bike_data ls;
     bikes
  | _ -> failwith "Expected List"
  
let () =
  let access_token = get_access_token in
  let _stations = get_stations access_token in
  let bikes = get_bikes access_token in
  List.iter
    (function Available n -> Printf.printf "%d " n | Maintenance _ -> ())
    (Hashtbl.find_all bikes 62);
