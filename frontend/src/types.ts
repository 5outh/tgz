import * as t from 'io-ts'

const Username = t.type({
  username: t.string
}, "Username")
export type Username = t.TypeOf<typeof Username>

const PlayerInfo = t.type(
  { username : Username
  , email    : t.string
  , playerId : t.number
  }, "PlayerInfo")
export type PlayerInfo = t.TypeOf<typeof PlayerInfo>

const Points = t.type(
  { points : t.number
  , step : t.number
  }, "Points")
export type Points = t.TypeOf<typeof Points>

const Rotated = <C extends t.Mixed>(codec: C) => t.union([t.type({Rotated: codec}), t.type({UnRotated: codec}) ], "Rotated")
const TechnologyCardState = t.type(
  { price : t.number
  , cattle : t.number
  }, "TechnologyCardState")
export type TechnologyCardState = t.TypeOf<typeof TechnologyCardState>

const Craftsman = t.keyof({
  potter: null,
  ivoryCarver: null,
  woodCarver: null,
  diamondCutter: null,
  vesselMaker: null,
  throneMaker: null,
  sculptor: null
}, "Craftsman")
export type Craftsman = t.TypeOf<typeof Craftsman>

const TechnologyCard = t.type(
  { name               : t.string
  , craftsmanType      : Craftsman
  , victoryRequirement : t.number
  , victoryPoints      : t.number
  , cost               : t.number
  }, "TechnologyCard")
export type TechnologyCard = t.TypeOf<typeof TechnologyCard>

const Activation = t.union([t.literal('BuilderActive'), t.literal('NomadsActive'), t.literal('None')], "Activation")
export type Activation = t.TypeOf<typeof Activation>

const Location = t.type({x: t.number, y: t.string}, "Location")
export type Location = t.TypeOf<typeof Location>

const Empire = t.union([
  t.literal("kilwa")
  , t.literal("mutapa")
  , t.literal("zulu")
  , t.literal("lozi")
  , t.literal("mapungubwe")
  ], "Empire")
export type Empire = t.TypeOf<typeof Empire>

const Specialist = t.keyof({
  Shaman: null,
  RainCeremony: null,
  Herd: null,
  Builder: null,
  Nomads: null
  }, "Specialist")
export type Specialist = t.TypeOf<typeof Specialist>

const God = t.keyof({
  Shadipinyi: t.any ,
  Elegua: t.any ,
  Dziva: t.any ,
  Eshu: t.any ,
  Gu: t.any ,
  Obatala: t.any ,
  Atete: t.any ,
  TsuiGoab: t.any ,
  Anansi: t.any ,
  Qamata: t.number ,
  Engai : t.any ,
  Xango  : t.any ,
}, "God")
export type God = t.TypeOf<typeof God>

const Player = t.type(
  { info               : t.union([PlayerInfo,  t.null])
  , victoryRequirement : Points
  , victoryPoints      : Points
  , empire             : t.union([Empire, t.null])
  , cattle             : t.number
  , monuments          : t.array(t.record(Location, t.number))
  , craftsmen          : t.array(t.record(Location, Rotated(Craftsman)))
  , technologyCards    : t.array(t.record(TechnologyCard, TechnologyCardState))
  , specialists        : t.array(t.record(Specialist, t.number))
  , god                : t.union([God , t.null])
  , activations        : t.array(Activation)
  }, "Player")
export type Player = t.TypeOf<typeof Player>

const EmpirePlaque = t.union([t.type({PlayerPlaque: Empire}), t.type({ShadipinyiPlaque: t.any})], "EmpirePlaque")
export type EmpirePlaque = t.TypeOf<typeof EmpirePlaque>

const GenerosityOfKingsState = t.type(
  { plaques       : t.array( EmpirePlaque )
  , cattlePool    : t.number
  , lastBid       : t.union([t.number , t.null])
  , playersPassed : t.array( t.number )
  }, "GenerosityOfKingsState")
export type GenerosityOfKingsState = t.TypeOf<typeof GenerosityOfKingsState>

const Phase = t.keyof({
  "PreSetup" : null,
  "Setup": null,
  "GenerosityOfKings": null,
  "ReligionAndCulture": null,
  "Revenues": null,
  "LetUsCompareMythologies": null
}, "Phase")
export type Phase = t.TypeOf<typeof Phase>

const Round = t.type(
  { players                : t.array( t.number )
  , current_player          : t.union([t.number , t.null])
  , used_markers            : t.array(t.record(Location, t.number))
  , generosity_of_kings_state : GenerosityOfKingsState
  , current_phase           : t.union([Phase , t.null])
  , step : t.number
  }, "Round")
export type Round = t.TypeOf<typeof Round>

const Resource = t.keyof({ Clay: null, Wood: null , Ivory: null, Diamonds: null }, "Resource")
export type Resource = t.TypeOf<typeof Resource>

const Land = t.keyof({
    StartingArea : t.any,
    BlankLand : t.any,
    Resource : Resource
  }, "Land")
export type Land = t.TypeOf<typeof Land>

const Square = t.keyof({
  Water: t.any,
  Land: Land
}, "Square")
export type Square = t.TypeOf<typeof Square>

  const MapLayout = t.type({ mapLayout : t.array(t.record(Location, Square)) }, "MapLayout")
export type MapLayout = t.TypeOf<typeof MapLayout>

export const GameV = t.type(
  { players   : t.array(t.record(t.number, Player))
  , round     : Round
  , mapLayout : MapLayout
  , technology_cards : t.array(t.record(Craftsman, t.array(TechnologyCard)))
  , gods : t.array( God )
  , specialists : t.array( Specialist )
  , winner    : t.union([t.number , t.null])
  , step : t.number
  , resource_tiles : t.array(t.record(Resource, t.number))
  , water_tiles : t.number
  , craftsman_tiles : t.array(t.record(Craftsman, t.number))
  }, "Game")
export type Game = t.TypeOf<typeof GameV>

export const GameViewV = t.type({
  id: t.number,
  name: t.string,
  state: GameV,
}, "GameView")
export type GameView = t.TypeOf<typeof GameViewV>
