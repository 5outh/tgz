import * as t from 'io-ts'

const Username = t.type({
  username: t.string
})
export type Username = t.TypeOf<typeof Username>

const PlayerInfo = t.type(
  { username : Username
  , email    : t.string
  , playerId : t.number
  })
export type PlayerInfo = t.TypeOf<typeof PlayerInfo>

const Points = t.type(
  { points : t.number
  , step : t.number
  })
export type Points = t.TypeOf<typeof Points>

const Rotated = <C extends t.Mixed>(codec: C) => t.union([t.type({Rotated: codec}), t.type({UnRotated: codec}) ])

// const _Rotated = <C extends t.Mixed>(codec: C) => t.type({Rotated: codec})
// const _UnRotated = <C extends t.Mixed>(codec: C) => t.type({UnRotated: codec})
// const Rotated = <C extends t.Mixed>(codec: C) => t.union([_Rotated<C>,_UnRotated<C>])
// export type Rotated<T> = t.TypeOf<typeof Rotated<T>>

const TechnologyCardState = t.type(
  { price : t.number
  , cattle : t.number
  })
export type TechnologyCardState = t.TypeOf<typeof TechnologyCardState>

const Craftsman = t.keyof({
  potter: null,
  ivoryCarver: null,
  woodCarver: null,
  diamondCutter: null,
  vesselMaker: null,
  throneMaker: null,
  sculptor: null
})
export type Craftsman = t.TypeOf<typeof Craftsman>

const TechnologyCard = t.type(
  { name               : t.string
  , craftsmanType      : Craftsman
  , victoryRequirement : t.number
  , victoryPoints      : t.number
  , cost               : t.number
  })
export type TechnologyCard = t.TypeOf<typeof TechnologyCard>

const Activation = t.union([t.literal('BuilderActive'), t.literal('NomadsActive'), t.literal('None')])
export type Activation = t.TypeOf<typeof Activation>

const Location = t.type({x: t.number, y: t.string})
export type Location = t.TypeOf<typeof Location>


const Empire = t.union([
  t.literal("kilwa")
  , t.literal("mutapa")
  , t.literal("zulu")
  , t.literal("lozi")
  , t.literal("mapungubwe")
  ])
export type Empire = t.TypeOf<typeof Empire>

const Specialist = t.keyof({
  Shaman: null,
  RainCeremony: null,
  Herd: null,
  Builder: null,
  Nomads: null
  })
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
})
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
  })
export type Player = t.TypeOf<typeof Player>

const EmpirePlaque = t.union([t.type({PlayerPlaque: Empire}), t.type({ShadipinyiPlaque: t.any})])
export type EmpirePlaque = t.TypeOf<typeof EmpirePlaque>

const GenerosityOfKingsState = t.type(
  { plaques       : t.array( EmpirePlaque )
  , cattlePool    : t.number
  , lastBid       : t.union([t.number , t.null])
  , playersPassed : t.array( t.number )
  })
export type GenerosityOfKingsState = t.TypeOf<typeof GenerosityOfKingsState>

const Phase = t.keyof({
  "PreSetup" : null,
  "Setup": null,
  "GenerosityOfKings": null,
  "ReligionAndCulture": null,
  "Revenues": null,
  "LetUsCompareMythologies": null
})
export type Phase = t.TypeOf<typeof Phase>

const Round = t.type(
  { players                : t.array( t.number )
  , currentPlayer          : t.union([t.number , t.null])
  , usedMarkers            : t.array(t.record(Location, t.number))
  , generosityOfKingsState : GenerosityOfKingsState
  , currentPhase           : t.union([Phase , t.null])
  , step : t.number
  })
export type Round = t.TypeOf<typeof Round>

const Resource = t.keyof({ Clay: null, Wood: null , Ivory: null, Diamonds: null })
export type Resource = t.TypeOf<typeof Resource>

const Land = t.keyof({
    StartingArea : t.any,
    BlankLand : t.any,
    Resource : Resource
  })
export type Land = t.TypeOf<typeof Land>

const Square = t.keyof({
  Water: t.any,
  Land: Land
})
export type Square = t.TypeOf<typeof Square>

const MapLayout = t.type({ mapLayout : t.array(t.record(Location, Square)) })
export type MapLayout = t.TypeOf<typeof MapLayout>

const Game = t.type(
  { players   : t.array(t.record(t.number, Player))
  , round     : Round
  , mapLayout : MapLayout
  , technologyCards : t.array(t.record(Craftsman, t.array(TechnologyCard)))
  , gods : t.array( God )
  , specialists : t.array( Specialist )
  , winner    : t.union([t.number , t.null])
  , step : t.number
  , resourceTiles : t.array(t.record(Resource, t.number))
  , waterTiles : t.number
  , craftsmanTiles : t.array(t.record(Craftsman, t.number))
  })
export type Game = t.TypeOf<typeof Game>
