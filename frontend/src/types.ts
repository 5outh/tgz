import * as t from 'io-ts'

const PlayerInfo = t.type(
  { username : t.string
  , email    : t.string
  , player_id : t.number
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
  Potter: null,
  IvoryCarver: null,
  WoodCarver: null,
  DiamondCutter: null,
  VesselMaker: null,
  ThroneMaker: null,
  Sculptor: null
}, "Craftsman")
export type Craftsman = t.TypeOf<typeof Craftsman>

const TechnologyCard = t.type(
  { name               : t.string
  , craftsman_type      : Craftsman
  , victory_requirement : t.number
  , victory_points      : t.number
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

  const God = t.union([
    t.type({Shadipinyi: t.any}),
    t.type({Elegua: t.any}),
    t.type({Dziva: t.any}),
    t.type({Eshu: t.any}),
    t.type({Gu: t.any}),
    t.type({Obatala: t.any}),
    t.type({Atete: t.any}),
    t.type({TsuiGoab: t.any}),
    t.type({Anansi: t.any}),
    t.type({Qamata: t.number}),
    t.type({Engai: t.any}),
    t.type({Xango: t.any})
  ], "God")
export type God = t.TypeOf<typeof God>

const Player = t.type(
  { info               : t.union([PlayerInfo,  t.null])
  , victory_requirement : Points
  , victory_points      : Points
  , empire             : t.union([Empire, t.null])
  , cattle             : t.number
  , monuments          : t.array(t.tuple([Location, t.number]))
  , craftsmen          : t.array(t.tuple([Location, Rotated(Craftsman)]))
  , technology_cards    : t.array(t.tuple([TechnologyCard, TechnologyCardState]))
  , specialists        : t.array(t.tuple([Specialist, t.number]))
  , god                : t.union([God , t.null])
  , activations        : t.array(Activation)
  }, "Player")
export type Player = t.TypeOf<typeof Player>

const EmpirePlaque = t.union([t.type({PlayerPlaque: Empire}), t.type({ShadipinyiPlaque: t.any})], "EmpirePlaque")
export type EmpirePlaque = t.TypeOf<typeof EmpirePlaque>

const GenerosityOfKingsState = t.type(
  { plaques       : t.array( EmpirePlaque )
  , cattle_pool    : t.number
  , last_bid       : t.union([t.number , t.null])
  , players_passed : t.array( t.number )
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

  const Land = t.union([
    t.type({StartingArea : t.any}),
    t.type({BlankLand : t.any}),
    t.type({Resource : Resource})
  ], "Land")
export type Land = t.TypeOf<typeof Land>

  const Square = t.union([
    t.type({Water: t.any}),
    t.type({Land: Land})
  ], "Square")
export type Square = t.TypeOf<typeof Square>

const MapLayout = t.type({ map_layout : t.array(t.tuple([Location, Square])) }, "MapLayout")
export type MapLayout = t.TypeOf<typeof MapLayout>

export const GameV = t.type(
  { players   : t.record(t.string, Player)
  , round     : Round
  , map_layout : t.array(t.tuple([Location, Square]))
  , technology_cards : t.array(t.tuple([Craftsman, t.array(TechnologyCard)]))
  , gods : t.array( God )
  , specialists : t.array( Specialist )
  , winner    : t.union([t.number , t.null])
  , step : t.number
  , resource_tiles : t.array(t.tuple([Resource, t.number]))
  , water_tiles : t.number
  , craftsman_tiles : t.array(t.tuple([Craftsman, t.number]))
  }, "Game")
export type Game = t.TypeOf<typeof GameV>

export const GameViewV = t.type({
  id: t.number,
  name: t.string,
  state: GameV,
}, "GameView")
export type GameView = t.TypeOf<typeof GameViewV>
