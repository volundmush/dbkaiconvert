#include "comm.h"
#include "utils.h"
#include "dg_scripts.h"
#include "constants.h"
#include "genolc.h"
#include "dg_event.h"
#include "maputils.h"
#include <filesystem>
#include <memory>
#include <iostream>
#include <vector>
#include <fmt/format.h>
#include <tuple>
#include "SQLiteCpp/SQLiteCpp.h"
#include <boost/algorithm/string.hpp>
#include <fstream>
#include "class.h"
#include "races.h"
#include "players.h"
#include "spells.h"
#include "nlohmann/json.hpp"
#include "dbatk/base.h"
#include "spdlog/spdlog.h"
#include "dbatk/database.h"
#include "dbatk/api.h"
#include "dbatk/components.h"
#include "dbatk/color.h"
#include "dbatk/dgscript.h"
#include "dbatk/zone.h"
#include "dbatk/core.h"


using namespace dbat;

struct old_ship_data {
    std::string name;
    std::set<RoomId> vnums;
    std::optional<RoomId> hatch_room{};
    std::optional<RoomId> ship_obj{};
    std::optional<RoomId> location{};
};

static struct old_ship_data gships[] = {
        {"Falcon", {3900, 3901, 3902, 3903, 3904}, 3900, 3900, 408},
        {"Simurgh", {3905, 3996, 3997, 3998, 3999}, 3905, 3905, 12002},
        {"Zypher", {3906, 3907, 3908, 3909, 3910}, 3906, 3906, 4250},
        {"Valkyrie", {3911, 3912, 3913, 3914, 3915}, 3911, 3911, 2323},
        {"Phoenix", {3916, 3917, 3918, 3919, 3920}, 3916, 3916, 408},
        {"Merganser", {3921, 3922, 3923, 3924, 3925}, 3921, 3921, 2323},
        {"Wraith", {3926, 3927, 3928, 3929, 3930}, 3930, 3930, 11626},
        {"Ghost", {3931, 3932, 3933, 3934, 3935}, 3935, 3935, 8194},
        {"Wisp", {3936, 3937, 3938, 3939, 3940}, 3940, 3940, 12002},
        {"Eagle", {3941, 3942, 3943, 3944, 3945}, 3945, 3945, 4250},

        {"Spectral", {3946, 3947, 3948, 3949, 3950}, 3950, {}, {}},
        {"Raven", {3951, 3952, 3953, 3954, 3955, 3961}, 3955, {}, {}},
        {"Marauder", {3956, 3957, 3958, 3959, 3960}, 3960, {}, {}},
        {"Vanir", {3962, 3963, 3964, 3965}, 3965, {}, {}},
        {"Aesir", {3966, 3967, 3968, 3969, 3970}, 3970, {}, {}},
        {"Undine", {3971, 3972, 3973, 3974, 3975}, 3975, {}, {}},
        {"Banshee", {3976, 3977, 3978, 3979, 3980}, 3980, {}, {}},
        {"Hydra", {3981, 3982, 3983, 3984, 3985}, 3985, {}, {}},
        {"Medusa", {3986, 3987, 3988, 3989, 3990}, 3990, {}, {}},
        {"Pegasus", {3991, 3992, 3993, 3994, 3995}, 3995, {}, {}},
        {"Zel 1", {5824}, 5824, {}, {}},
        {"Zel 2", {5825}, 5825, {}, {}}
};

static struct old_ship_data customs[] = {
        {"Yun-Yammka", {19900, 19901, 19902}, 19901, 19900, {}},
        {"The Dark Archon", {19903, 19912, 19913, 19914}, 19912, 19916, {}},
        {"The Zafir Krakken", {19904, 19905, 19906, 19907}, 19905, 19905, {}},
        {"Crimson Talon", {19908, 19909, 19910, 19911}, 19908, 19910, {}},
        {"Rust Bucket", {19915, 19916, 19918, 19930}, 19915, 19921, {}},
        {"The Adamant", {19917, 19949, 19955, 19956}, 19949, 19966, {}},
        {"Vanguard", {19919, 19920, 19921, 19922}, 19920, 19926, {}},
        {"The Glacial Shadow", {19925, 19923, 19924, 19926}, 19923, 19931, {}},
        {"The Molecular Dynasty", {19927, 19928, 19929, 19954}, 19927, 19936, {}},
        {"Poseidon", {19931, 19933, 19932, 19934}, 19931, 19941, {}},
        {"Sakana Mirai", {19935, 19936, 19937, 19938}, 19935, 19946, {}},
        {"Earth Far Freighter Enterjection", {19939, 19940, 19941, 19942}, 19939, 19951, {}},
        {"Soaring Angel", {19943, 19944, 19945, 19946}, 19943, 19956, {}},
        {"The Grey Wolf", {19947, 19948, 19978, 19979}, 19947, 19961, {}},
        {"The Event Horizon", {19950, 19951, 19980, 19981}, 19950, 19971, {}},
        {"Fleeting Star", {19952, 19953, 19957, 19958}, 19952, 19976, {}},
        {"Makenkosappo", {19959, 19960, 19961, 19962}, 19959, 19981, {}},
        {"The Nightingale", {19963, 19964, 19965, 19982}, 19963, 19986, {}},
        {"The Honey Bee", {19966, 19967, 19968, 19969}, 19966, 19991, {}},
        {"The Bloodrose", {19970, 19971, 19972, 19973}, 19970, 19996, {}},
        {"The Stimato", {19974, 19975, 19976, 19977}, 19974, {}, {}},
        {"The Tatsumaki", {15805, 15806, 15807, 15808}, 15805, 15805, {}},
        {"Shattered Soul", {15800, 15801, 15802, 15803}, 15800, {}, {}}
};

void dump_zones() {

    for(const auto& [id, zone] : zone_table) {
        auto &z = zones[id];
        z.id = id;
        z.name = std::string(zone.name);
        z.builders = std::string(zone.builders);
        z.lifespan = zone.lifespan;
        z.age = zone.age;
        z.reset_mode = zone.reset_mode;
        z.top = zone.top;
        z.bot = zone.bot;

        for(const auto &c : zone.cmd) {
            auto &nc = z.cmds.emplace_back();
            nc.command = c.command;
            nc.if_flag = c.if_flag;
            nc.arg1 = c.arg1;
            nc.arg2 = c.arg2;
            nc.arg3 = c.arg3;
            nc.arg4 = c.arg4;
            nc.arg5 = c.arg5;
            if(c.sarg1) nc.sarg1 = c.sarg1;
            if(c.sarg2) nc.sarg2 = c.sarg2;
        }

        for(auto i = 0; i < 4; i++) {
            if(IS_SET_AR(zone.zone_flags, i)) {
                z.flags[i] = true;
            }
        }
    }
}

void dump_scripts() {

    for(const auto& [id, trig] : trig_index) {
        auto t = std::make_shared<DgScriptPrototype>();
        auto p = trig.proto;
        t->id = id;
        dgScripts[id] = t;
        t->name = p->name;
        t->scriptType = p->attach_type;
        t->narg = p->narg;

        if(boost::icontains(p->name, "roshi")) {
            log("doing guild guard!");
        }

        for(auto i = 0; i < 32; i++) {
            // scan p->trigger_type as a bitset and set the appropriate flags
            if(IS_SET(p->trigger_type, (1 <<i))) {
                t->triggerType[i] = true;
            }
        }

        std::string script_body;
        for(auto cmd = p->cmdlist; cmd; cmd = cmd->next) {
            t->lines.emplace_back(cmd->cmd);
        }

        if(p->arglist) t->arglist = p->arglist;
    }
}

static std::string cleanString(const std::string &txt) {
    // Given a txt, we want to remove all trailing whitespace/newlines and also the sequence "@n".
    // Then, if the trimmed text contains a @ at all, we want to append a "@n" at the end.
    std::string str = txt;
    for(auto i = 0; i < 3; i++) {
        boost::trim_right(str);
        if(str.ends_with("@n")) {
            str = str.substr(0, str.size() - 2);
        }
    }

    // if '@' is still present in the text, append "@n"
    if (str.find('@') != std::string::npos) {
        str.append("@n");
    }

    return str;
}

nlohmann::json dump_item(obj_data *obj) {

    nlohmann::json j;

    if (obj->name && strlen(obj->name)) j["Keywords"] = obj->name;
    if (obj->short_description && strlen(obj->short_description)) j["Name"] = cleanString(obj->short_description);
    if (obj->look_description && strlen(obj->look_description))  j["LookDescription"] = cleanString(obj->look_description);
    if (obj->room_description && strlen(obj->room_description)) j["RoomDescription"] = cleanString(obj->room_description);

    if(obj->level) j["LevelRequirement"] = obj->level;
    if(obj->weight) j["Weight"] = obj->weight;
    if(obj->cost) j["Cost"] = obj->cost;
    if(obj->size != SIZE_UNDEFINED) j["Size"] = obj->size;

    std::vector<std::pair<std::string, std::string>> extraDescriptions;
    for (auto ex = obj->ex_description; ex; ex = ex->next) {
        if (!(ex->keyword && ex->description)) continue;
        j["ExtraDescriptions"].push_back({cleanString(std::string(ex->keyword)), cleanString(std::string(ex->description))});
    }

    nlohmann::json dg;
    for (auto t = obj->proto_script; t; t = t->next) {
        dg["scripts"].push_back(t->vnum);
    }
    if(dg.size()) j["DgScripts"] = dg;

    if(!IS_SET_AR(obj->wear_flags, ITEM_WEAR_TAKE)) {
        j["ItemFlags"].push_back(iflags::NOTAKE);
    }

    for (auto i = 1; i < NUM_ITEM_WEARS; i++) {
        if (IS_SET_AR(obj->wear_flags, i)) {
            j["CharWornData"].push_back(i-1);
        }
    }

    for (auto i = 0; i < NUM_ITEM_FLAGS; i++) {
        if (IS_SET_AR(obj->extra_flags, i)) {
            j["ItemFlags"].push_back(i);
        }
    }

    for (auto i = 0; i < NUM_AFF_FLAGS; i++) {
        if (IS_SET_AR(obj->bitvector, i)) {
            j["ItemAffects"].push_back(i);
        }
    }

    j["Material"] = obj->value[VAL_ALL_MATERIAL];
    nlohmann::json d;
    d["durability"] = obj->value[VAL_ALL_HEALTH];
    d["maxDurability"] = obj->value[VAL_ALL_MAXHEALTH];
    j["Durability"] = d;


    nlohmann::json l, w, c, dr, f;
    // Gotta convert the specific types of items and their shenanigans here...
    switch(obj->type_flag) {
        case ITEM_LIGHT:
            l["timeLeft"] = obj->value[VAL_LIGHT_TIME];
            l["hoursLeft"] = obj->value[VAL_LIGHT_HOURS];
            j["LightData"] = l;
            break;
        case ITEM_WEAPON:
            w["skill"] = obj->value[VAL_WEAPON_SKILL];
            w["damDice"] = obj->value[VAL_WEAPON_DAMDICE];
            w["damSize"] = obj->value[VAL_WEAPON_DAMSIZE];
            w["damType"] = obj->value[VAL_WEAPON_DAMTYPE];
            j["WeaponData"] = w;
            break;
        case ITEM_CONTAINER:
            c["capacity"] = obj->value[VAL_CONTAINER_CAPACITY];
            c["flags"] = obj->value[VAL_CONTAINER_FLAGS];
            j["ContainerData"] = c;
            break;
        case ITEM_DRINKCON:
            dr["capacity"] = obj->value[VAL_DRINKCON_CAPACITY];
            dr["current"] = obj->value[VAL_DRINKCON_HOWFULL];
            dr["liquid"] = obj->value[VAL_DRINKCON_LIQUID];
            if(obj->value[VAL_DRINKCON_POISON]) {
                dr["poison"] = true;
            }
            j["DrinkData"] = dr;
            break;
        case ITEM_FOOD:
            f["maxNutrition"] = obj->value[VAL_FOOD_FOODVAL];
            f["nutrition"] = obj->value[VAL_FOOD_FOODVAL];
            if(obj->value[VAL_FOOD_POISON]) {
                f["poisoned"] = true;
            }
            j["FoodData"] = f;
        default:
            break;
    }

    for (auto & i : obj->affected) {
        if (i.location == APPLY_NONE) {
            continue;
        }
        j["ItemModifiers"].push_back({i.location, i.specific, i.modifier});
    }
    j["ItemVnum"] = obj->vn;
    j["Item"] = true;
    return j;
}

void dump_items() {

    SQLite::Statement q1(*db, "INSERT INTO prototypes (name, data) VALUES (?,?);");

    for(auto &[vnum, obj] : obj_proto) {
        auto key = fmt::format("item:{}", vnum);
        q1.bind(1, key);
        auto data = dump_item(&obj);
        data["Prototype"] = key;
        q1.bind(2, data.dump(4, ' ', false, nlohmann::json::error_handler_t::ignore));
        q1.exec();
        q1.reset();
    }
}


nlohmann::json dump_character(char_data *ch) {
    auto is_npc = IS_NPC(ch);
    nlohmann::json j;
    if (ch->name && strlen(ch->name)) j["Keywords"] = cleanString(ch->name);
    if (ch->short_description && strlen(ch->short_description)) j["Name"] = cleanString(ch->short_description);
    if (ch->look_description && strlen(ch->look_description)) j["LookDescription"] = cleanString(ch->look_description);
    if (ch->room_description && strlen(ch->room_description)) j["RoomDescription"] = cleanString(ch->room_description);

    int race = 0;

    switch(ch->race->getID()) {
        case ::race::spirit:
            race = 0;
            break;
        default:
            race = ch->race->getID() + 1;
            break;
    }
    j["Race"] = race;

    int sensei = 0;

    switch(ch->chclass->getID()) {
        case ::sensei::commoner:
            sensei = 0;
            break;
        default:
            sensei = ch->chclass->getID() + 1;
            break;
    }
    j["Sensei"] = sensei;
    j["Sex"] = ch->sex;
    j["Weight"] = ch->weight;
    j["Height"] = ch->height;
    j["Position"] = ch->position;

    if(ch->size != SIZE_UNDEFINED) j["Size"] = ch->size;
    std::array<double, cstat::numCharStats> stats{};

    if(ch->level) stats[cstat::LEVEL] = ch->level;
    if(ch->exp) stats[cstat::EXPERIENCE] = ch->exp;
    if(ch->alignment) stats[cstat::ALIGNMENT] = ch->alignment;
    if(ch->armor) stats[cstat::ARMOR] =  ch->armor;
    if(ch->gold) j["Money"] = ch->gold;

    if(ch->real_abils.str) stats[cstat::STRENGTH] = ch->real_abils.str;
    if(ch->real_abils.intel) stats[cstat::INTELLIGENCE] = ch->real_abils.intel;
    if(ch->real_abils.wis) stats[cstat::WISDOM] = ch->real_abils.wis;
    if(ch->real_abils.dex) stats[cstat::DEXTERITY] = ch->real_abils.dex;
    if(ch->real_abils.con) stats[cstat::CONSTITUTION] = ch->real_abils.con;
    if(ch->real_abils.cha) stats[cstat::SPEED] = ch->real_abils.cha;

    if(ch->basepl) stats[cstat::POWERLEVEL] = ch->basepl;
    if(ch->basest) stats[cstat::STAMINA] = ch->basest;
    if(ch->baseki) stats[cstat::KI] = ch->baseki;

    nlohmann::json dg;
    for (auto t = ch->proto_script; t; t = t->next) {
        dg["scripts"].push_back(t->vnum);
    }
    if(dg.size()) j["DgScripts"] = dg;

    if(is_npc) for(auto i = 0; i < NUM_MOB_FLAGS; i++) {
            if(!IS_SET_AR(ch->act, i)) {
                continue;
            }
            j["MobFlags"].push_back(i);
        }

    for(auto i = 0; i < NUM_AFF_FLAGS; i++) {
        if(!IS_SET_AR(ch->affected_by, i)) {
            continue;
        }
        j["AffectFlags"].push_back(i);
    }


    for(auto i = 0; i < cstat::numCharStats; i++) {
        if(stats[i] != 0.0) {
            j["CharacterStats"].push_back(std::make_pair(i, stats[i]));
        }
    }

    j["Character"] = true;
    if(is_npc) {
        j["NPCVnum"] = ch->vn;
        j["NPC"] = true;
    }

    return j;
}

void dump_npc() {
    SQLite::Statement q1(*db, "INSERT INTO prototypes (name, data) VALUES (?,?);");

    for (auto &[vnum, npc]: mob_proto) {
        auto key = fmt::format("npc:{}", vnum);
        q1.bind(1, key);
        auto data = dump_character(&npc);
        data["Prototype"] = key;
        q1.bind(2, data.dump(4, ' ', false, nlohmann::json::error_handler_t::ignore));
        q1.exec();
        q1.reset();
    }
}

static std::set<long> player_ids;

OpResult<ObjectId> dump_player(const char *name, int64_t acc) {

    auto *ch = new char_data();
    ch->player_specials = new player_special_data();
    if(load_char(name, ch) < 0) {
        log("Error loading player %s", name);
        free_char(ch);
        return {{}, "Error loading player"};
    }
    if(player_ids.count(ch->idnum)) {
        log("Duplicate player %s", name);
        free_char(ch);
        return {{}, "Duplicate player"};
    }
    player_ids.insert(ch->idnum);
    log("Dumping player %d: %s", ch->idnum, ch->name);

    auto data = dump_character(ch);
    nlohmann::json jacc;
    jacc["accountId"] = acc;
    data["Player"] = jacc;

    for(auto i = 0; i < NUM_PLR_FLAGS; i++) {
        if(!IS_SET_AR(ch->act, i)) {
            continue;
        }
        data["PlayerFlags"].push_back(i);
    }

    for(auto i = 0; i < SKILL_TABLE_SIZE; i++) {
        if(GET_SKILL(ch, i) || GET_SKILL_BONUS(ch, i) || GET_SKILL_PERF(ch, i)) {
            dbat::skill_data s;
            s.level = GET_SKILL(ch, i);
            s.bonus = GET_SKILL_BONUS(ch, i);
            s.perfection = GET_SKILL_PERF(ch, i);
            data["Skills"].push_back(std::make_pair(i, s.serialize()));
        }
    }

    for (auto i = 0; i < 52; i++) {
        if(GET_BONUS(ch, i)) {

        };
    }

    free_char(ch);
    auto ent = createObject();
    deserializeEntity(ent, data);
    return {registry.get<ObjectId>(ent), std::nullopt};
}

void dump_accounts() {

    auto ufolder = std::filesystem::path("user");
    SQLite::Statement q1(*db, "INSERT INTO accounts (username, password, email, adminLevel) VALUES (?, ?, ?, ?)");
    SQLite::Statement q2(*db, "INSERT INTO playerCharacters (account, character) VALUES (?, ?)");
    for(auto &p : std::filesystem::recursive_directory_iterator(ufolder)) {
        if(!p.path().string().ends_with(".usr")) continue;
        std::ifstream f(p.path());
        std::string password, email, username;
        long rpp, supervisor_level, character_slots;

        std::getline(f, username);
        std::getline(f, email);
        std::getline(f, password);
        auto [res, hashed] = hashPassword(password);
        std::string phash = "";
        if(res) {
            phash = hashed.value();
        } else {
            log("Error hashing password for %s", username.c_str());
        }

        f >> character_slots;
        f >> rpp;

        //i.data["rpp"] = rpp;
        //i.data["character_slots"] = character_slots;

        std::vector<std::string> characters;
        for(int i = 0; i < 5; i++) {
            std::string line;
            std::getline(f, line);
            if(line == "Empty") continue;
            if(line.empty()) continue;
            characters.push_back(line);
        }

        f >> supervisor_level;
        int b;
        f >> b;
        f >> b;

        q1.bind(1, username);
        q1.bind(2, phash);
        q1.bind(3, email);
        q1.bind(4, supervisor_level);
        q1.exec();
        auto accID = db->getLastInsertRowid();
        q1.reset();


        for(auto &c : characters) {
            auto [obj, err] = dump_player(c.c_str(), accID);
            if(err.has_value()) {
                log("Error dumping player %s", c.c_str());
                continue;
            }
            q2.bind(1, accID);
            q2.bind(2, static_cast<int64_t>(obj.index));
            q2.exec();
            q2.reset();
        }
    }
}

void dump_skills() {

    for(auto i = 0; i < SKILL_TABLE_SIZE; i++) {
        if(!spell_info[i].name) continue;
        if(!strcasecmp(spell_info[i].name, "!UNUSED!")) continue;

    }

}

struct StructureDef {
    std::string name;
    entt::entity parent{entt::null};
    std::set<std::size_t> roomFlags{};
    std::optional<std::pair<std::size_t, std::size_t>> roomRange;
    std::set<RoomId> roomIDs{}, roomSkips{};
    bool global{true};
};

static std::set<RoomId> unknowns;

template<typename... ComponentTypes>
entt::entity assembleStructure(const StructureDef &def) {
    auto obj = createObject();
    setName(obj, def.name);

    if(registry.valid(def.parent)) {
        setParent(obj, def.parent);
    }

    // Work some component magic here...
    (registry.emplace<ComponentTypes>(obj), ...);

    std::set<RoomId> rooms = def.roomIDs;

    if(def.roomRange) {
        for(auto i = def.roomRange.value().first; i <= def.roomRange.value().second; i++) {
            auto found = world.find(i);
            if(found == world.end()) continue;
            rooms.insert(i);
        }
    }

    if(!def.roomFlags.empty()) {
        for(auto &[vn, room] : world) {
            for(auto &f : def.roomFlags) {
                if(IS_SET_AR(room.room_flags, f)) {
                    rooms.insert(vn);
                    break;
                }
            }
        }
    }

    logger->info("Assembling Structure: {}, Rooms: {}", def.name, rooms.size());


    if(!rooms.empty()) {
        auto &of = registry.get_or_emplace<ObjectFlags>(obj);
        of.data[oflags::GLOBALROOMS] = def.global;

        auto &roomstorage = registry.get_or_emplace<Area>(obj);
        for(auto &vn : rooms) {
            if(legacyRooms.contains(vn)) continue;
            auto found = world.find(vn);
            if(found == world.end()) continue;
            auto &oldroom = found->second;

            nlohmann::json j, jr;
            jr["id"] = vn;
            jr["obj"] = getObjectId(obj);
            j["Room"] = jr;
            if(oldroom.name && strlen(oldroom.name)) j["Name"] = oldroom.name;
            if(oldroom.look_description && strlen(oldroom.look_description)) j["LookDescription"] = oldroom.look_description;
            for(auto i = 0; i < NUM_ROOM_FLAGS; i++) {
                if(IS_SET_AR(oldroom.room_flags, i)) j["RoomFlags"].push_back(i);
            }
            j["Terrain"] = oldroom.sector_type;

            for(auto i = 0; i < oldroom.dir_option.size(); i++) {
                auto &opt = oldroom.dir_option[i];
                if(!opt) continue;
                if(!world.contains(opt->to_room)) continue;

                nlohmann::json jdest;
                jdest["x"] = opt->to_room;
                j["Exits"].push_back(std::make_pair(i, jdest));

                nlohmann::json jopt;
                if(opt->general_description && strlen(opt->general_description)) jopt["description"] = opt->general_description;
                if(opt->keyword && strlen(opt->keyword)) jopt["keyword"] = opt->keyword;
                for(auto k = 0; k < NUM_EXIT_FLAGS; k++) if(IS_SET(opt->exit_info, (1 << k))) jopt["flags"].push_back(k);
                if(opt->key != NOTHING) jopt["legacyKey"] = opt->key;
                if(opt->dclock) jopt["dclock"] = opt->dclock;
                if(opt->dchide) jopt["dchide"] = opt->dchide;
                if(jopt.size() > 0) j["Doors"].push_back(std::make_pair(i, jopt));
            }

            auto rent = registry.create();
            roomstorage.data.emplace(vn, rent);
            deserializeEntity(rent, j);
            unknowns.erase(vn);
            if(def.global) {
                Location dest;
                dest.locationType == LocationType::Area;
                dest.data = obj;
                dest.x = vn;
                legacyRooms[vn] = dest;
            }
        }
    }

    return obj;

}

void migrate_objects() {

    StructureDef adef;
    adef.name = "Admin Land";
    adef.roomIDs = {0, 1, 2, 11, 14, 5, 6, 7, 16694, 16698, 8, 12, 9};
    auto admin_land = assembleStructure<Dimension>(adef);

    StructureDef mudschooldef;
    mudschooldef.name = "MUD School";
    mudschooldef.roomRange = {100, 155};
    auto mud_school = assembleStructure<Dimension>(mudschooldef);

    StructureDef mvdef;
    mvdef.name = "Multiverse";
    auto multiverse = assembleStructure<Dimension>(mvdef);

    StructureDef xvdef;
    xvdef.name = "Xenoverse";
    auto xenoverse = assembleStructure<Dimension>(xvdef);

    auto universe7 = assembleStructure<Dimension>({"Universe 7", multiverse});

    auto mortal_plane = assembleStructure<Dimension>({"Mortal Plane", universe7});
    auto celestial_plane = assembleStructure<Dimension>({"Celestial Plane", universe7});
    auto space = assembleStructure<Expanse>({"Depths of Space", mortal_plane});
    setLookDescription(space, world[20199].look_description);
    auto &exp = registry.get<Expanse>(space);
    exp.minX = -100;
    exp.maxX = 100;
    exp.minY = -100;
    exp.maxY = 100;
    exp.minZ = 0;
    exp.maxZ = 0;

    auto spaceFlags = {ROOM_EORBIT, ROOM_FORBIT, ROOM_KORBIT, ROOM_NORBIT, ROOM_VORBIT, ROOM_AORBIT, ROOM_YORBIT,
                       ROOM_KANORB, ROOM_ARLORB, ROOM_NEBULA, ROOM_ASTERO, ROOM_WORMHO, ROOM_STATION, ROOM_STAR, ROOM_CORBIT};

    auto checkFlags = [&](auto &room) {
        for(auto &f : spaceFlags) {
            if(IS_SET_AR(room.room_flags, f)) return true;
        }
        return false;
    };

    auto checkDirs = [&](auto &room) {
        for(auto &dir : {UP, DOWN, INDIR, OUTDIR}) {
            if(room.dir_option[dir]) return true;
        }
        return false;
    };

    auto &baseSpace = world[20199];

    auto checkText = [&](auto &room) {
        if(!boost::iequals(room.name, baseSpace.name)) return true;
        if(!boost::iequals(room.look_description, baseSpace.look_description)) return true;
        return false;
    };
    std::set<room_vnum> spaceRooms;
    // for this we need to scan mapnums, both rows and columns, and check the old "space" rooms at those ids.
    // Only if it's something particularly cool will we add it to the grid.
    int curX = -100, curY = 100;
    // We need to march from mapnums[0][0] to mapnums[0][200] for the first row, with respect to the altered coordinates.
    // As in, [0][0] is the top left of the space map in the old system, but [-100][100] is the top left of the space map in the new system.
    // Cartesian coordinates, which is easy to do 'cuz we're storing it in a std::unordered_map<GridPoint, entt::entity>
    for(auto &row : mapnums) {
        for(auto &col : row) {
            auto found = world.find(col);
            if(found == world.end()) continue;
            auto &room = found->second;
            spaceRooms.insert(col);
            // Okay let's analyze the room and see if it's worth adding to the grid.
            Location dest;
            dest.data = space;
            dest.x = curX;
            dest.y = curY;
            dest.z = 0.0;
            dest.locationType = LocationType::Expanse;
            legacyRooms.emplace(col, dest);
            GridPoint gp(curX, curY, 0);
            if(checkFlags(room) || checkDirs(room) || checkText(room)) {
                auto pent = registry.create();
                exp.poi.emplace(gp, pent);
                // Now we gotta set whatever's special about the PoI...
                if(!boost::iequals(room.name, baseSpace.name)) setName(pent, room.name);
                if(!boost::iequals(room.look_description, baseSpace.look_description)) setLookDescription(pent, room.look_description);
                // The second factor is possible flags...
                if(IS_SET_AR(room.room_flags, ROOM_EORBIT)) {
                    setRoomDescription(pent, "@GE@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_NORBIT)) {
                    setRoomDescription(pent, "@gN@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_KORBIT)) {
                    setRoomDescription(pent, "@mK@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_VORBIT)) {
                    setRoomDescription(pent, "@YV@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_AORBIT)) {
                    setRoomDescription(pent, "@BA@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_YORBIT)) {
                    setRoomDescription(pent, "@MY@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_KANORB)) {
                    setRoomDescription(pent, "@CK@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_ARLORB)) {
                    setRoomDescription(pent, "@mA@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_NEBULA)) {
                    setRoomDescription(pent, "@m&@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_ASTERO)) {
                    setRoomDescription(pent, "@yQ@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_WORMHO)) {
                    setRoomDescription(pent, "@1 @n");
                } else if(IS_SET_AR(room.room_flags, ROOM_STATION)) {
                    setRoomDescription(pent, "@DS@n");
                } else if(IS_SET_AR(room.room_flags, ROOM_STAR)) {
                    setRoomDescription(pent, "@6 @n");
                } else if(IS_SET_AR(room.room_flags, ROOM_CORBIT)) {
                    setRoomDescription(pent, "@MC@n");
                }

                // The third factor is possible exits...
                for(auto &dir : {UP, DOWN, INDIR, OUTDIR}) {
                    if(room.dir_option[dir]) {
                        auto &exits = registry.get_or_emplace<Exits>(pent);
                        Location dest;
                        dest.locationType = LocationType::Area;
                        dest.x = room.dir_option[dir]->to_room;
                        exits.data.emplace(static_cast<dir::DirectionId>(dir), dest);
                    };
                }
            }

            curX++;
        }
        curX = -100;
        curY--;
    }

    std::unordered_map<std::string, std::set<RoomId>> areas;

    for(auto &[rv, room] : world) {
        auto sense = sense_location_name(rv);
        if(sense == "Unknown.") {
            if(!spaceRooms.contains(rv))
                unknowns.insert(rv);
        } else {
            areas[sense].insert(rv);
        }
    }

    std::unordered_map<std::string, entt::entity> areaObjects;

    for(auto &[name, rooms] : areas) {
        StructureDef def;
        def.name = name;
        def.roomIDs = rooms;
        auto aent = assembleStructure<>(def);
        areaObjects[name] = aent;
    }

    auto planet_earth = assembleStructure<CelestialBody>({"Earth", space, {ROOM_EARTH}});
    auto planet_vegeta = assembleStructure<CelestialBody>({"Vegeta", space, {ROOM_VEGETA}});
    auto planet_frigid = assembleStructure<CelestialBody>({"Frigid", space, {ROOM_FRIGID}});
    auto planet_namek = assembleStructure<CelestialBody>({"Namek", space, {ROOM_NAMEK}});
    auto planet_konack = assembleStructure<CelestialBody>({"Konack", space, {ROOM_KONACK}});
    auto planet_aether = assembleStructure<CelestialBody>({"Aether", space, {ROOM_AETHER}});
    auto planet_yardrat = assembleStructure<CelestialBody>({"Yardrat", space, {ROOM_YARDRAT}});
    auto planet_kanassa = assembleStructure<CelestialBody>({"Kanassa", space, {ROOM_KANASSA}});
    auto planet_arlia = assembleStructure<CelestialBody>({"Arlia", space, {ROOM_ARLIA}});
    auto planet_cerria = assembleStructure<CelestialBody>({"Cerria", space, {ROOM_CERRIA}});


    auto moon_zenith = assembleStructure<CelestialBody>({"Zenith", space});
    for(const auto& name : {"Ancient Castle", "Utatlan City", "Zenith Jungle"}) {
        setParent(areaObjects[name], moon_zenith);
    }
    auto &cel = registry.get_or_emplace<CelestialBody>(moon_zenith);
    cel.type = celtype::MOON;

    StructureDef ucdef;
    ucdef.name = "Underground Cavern";
    ucdef.parent = moon_zenith;
    ucdef.roomRange = {62900, 63000};
    auto underground_cavern = assembleStructure<>(ucdef);

    for(auto &p : {planet_earth, planet_aether, planet_namek, moon_zenith}) {
        auto &cel = registry.get_or_emplace<CelestialBody>(p);
        cel.flags[celflags::ETHERSTREAM] = true;
    }

    StructureDef hbtcdef;
    hbtcdef.name = "Hyperbolic Time Chamber";
    hbtcdef.parent = universe7;
    hbtcdef.roomRange = {64000, 64098};
    auto hbtc = assembleStructure(hbtcdef);
    registry.emplace<Dimension>(hbtc);

    StructureDef bodef;
    bodef.name = "The Black Omen";
    bodef.parent = space;
    bodef.roomIDs.insert(19053);
    bodef.roomIDs.insert(19039);
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Black Omen")) bodef.roomIDs.insert(rv);
    }
    auto black_omen = assembleStructure<Vehicle>(bodef);
    auto &bproto = obj_proto[62501];
    setName(black_omen, bproto.name);
    if(bproto.look_description) setLookDescription(black_omen, bproto.look_description);
    if(bproto.room_description) setRoomDescription(black_omen, bproto.room_description);
    if(bproto.short_description) setShortDescription(black_omen, bproto.short_description);

    std::unordered_map<rflags::RFlagId, entt::entity> planetMap = {
            {rflags::EARTH, planet_earth},
            {rflags::VEGETA, planet_vegeta},
            {rflags::FRIGID, planet_frigid},
            {rflags::NAMEK, planet_namek},
            {rflags::YARDRAT, planet_yardrat},
            {rflags::KONACK, planet_konack},
            {rflags::AETHER, planet_aether},
            {rflags::KANASSA, planet_kanassa},
            {rflags::ARLIA, planet_arlia},
            {rflags::CERRIA, planet_cerria},
    };

    logger->info("Attempting to deduce Areas to Planets...");
    auto v = registry.view<Room, RoomFlags>();
    for(auto &ent : v) {
        // check for planetMap flags and, if found, bind the area this room belongs to, to the respective planet.
        auto &room = registry.get<Room>(ent);
        auto &flags = registry.get<RoomFlags>(ent);
        for(auto &p : planetMap) {
            if(flags.data[p.first]) {
                auto e = room.obj.getObject();
                setParent(e, p.second);
                break;
            }
        }
    }
    logger->info("Done deducing Areas to Planets.");


    StructureDef nodef;
    nodef.name = "Northran";
    nodef.parent = xenoverse;
    nodef.roomRange = {17900, 18000};
    auto northran = assembleStructure<Dimension>(nodef);

    StructureDef celdef;
    celdef.name = "Celestial Corp";
    celdef.parent = space;
    celdef.roomRange = {16305, 16399};
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Celestial Corp")) celdef.roomIDs.insert(rv);
    }
    auto celestial_corp = assembleStructure<Vehicle>(celdef);

    StructureDef gneb;
    gneb.name = "Green Nebula Mall";
    gneb.parent = space;
    gneb.roomRange = {17264, 17277};
    auto green_nebula = assembleStructure<Vehicle>(gneb);

    StructureDef cooler;
    cooler.name = "Cooler's Ship";
    cooler.parent = space;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Cooler's Ship")) cooler.roomIDs.insert(rv);
    }
    auto cooler_ship = assembleStructure<Vehicle>(cooler);

    StructureDef alph;
    alph.name = "Alpharis";
    alph.parent = space;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Alpharis")) alph.roomIDs.insert(rv);
    }
    auto alpharis = assembleStructure<Vehicle>(alph);

    StructureDef dzone;
    dzone.name = "Dead Zone";
    dzone.parent = universe7;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Dead Zone")) dzone.roomIDs.insert(rv);
    }
    auto dead_zone = assembleStructure<Dimension>(dzone);

    StructureDef bast;
    bast.name = "Blasted Asteroid";
    bast.parent = space;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Blasted Asteroid")) bast.roomIDs.insert(rv);
    }
    auto blasted_asteroid = assembleStructure<CelestialBody>(bast);
    {
        auto &celbod = registry.get_or_emplace<CelestialBody>(blasted_asteroid);
        celbod.type = celtype::ASTEROID;
    }

    StructureDef listres;
    listres.name = "Lister's Restaurant";
    listres.parent = xenoverse;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Lister's Restaurant")) listres.roomIDs.insert(rv);
    }
    auto listers_restaurant = assembleStructure<Dimension>(listres);

    StructureDef scasino;
    scasino.name = "Shooting Star Casino";
    scasino.parent = xenoverse;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "Shooting Star Casino")) scasino.roomIDs.insert(rv);
    }
    auto shooting_star_casino = assembleStructure<Dimension>(scasino);

    StructureDef outdef;
    outdef.name = "The Outpost";
    outdef.parent = celestial_plane;
    for(auto &rv : unknowns) {
        if(boost::icontains(stripAnsi(world[rv].name), "The Outpost")) outdef.roomIDs.insert(rv);
    }
    auto outpost = assembleStructure<>(outdef);

    StructureDef kyem;
    kyem.name = "King Yemma's Domain";
    kyem.parent = celestial_plane;
    kyem.roomRange = {6000, 6031};
    kyem.roomSkips.insert(6017);
    auto king_yemma = assembleStructure<>(kyem);

    StructureDef snway;
    snway.name = "Snake Way";
    snway.parent = celestial_plane;
    snway.roomRange = {6031, 6100};
    snway.roomIDs.insert(6017);
    auto snake_way = assembleStructure<>(snway);

    StructureDef nkai;
    nkai.name = "North Kai's Planet";
    nkai.parent = celestial_plane;
    nkai.roomRange = {6100, 6139};
    auto north_kai = assembleStructure<CelestialBody>(nkai);
    {
        auto &celbod = registry.get_or_emplace<CelestialBody>(north_kai);
        auto &grav = registry.get_or_emplace<Gravity>(north_kai);
        grav.data = 10.0;
    }

    StructureDef serp;
    serp.name = "Serpent's Castle";
    serp.parent = snake_way;
    serp.roomRange = {6100, 6167};
    auto serpents_castle = assembleStructure<>(serp);

    StructureDef gkai;
    gkai.name = "Grand Kai's Planet";
    gkai.parent = celestial_plane;
    gkai.roomRange = {6800, 6961};

    StructureDef maze;
    maze.name = "Maze of Echoes";
    maze.parent = celestial_plane;
    maze.roomRange = {7100, 7200};
    auto maze_of_echoes = assembleStructure<>(maze);

    StructureDef cat;
    cat.name = "Dark Catacomb";
    cat.parent = maze_of_echoes;
    cat.roomRange = {7200, 7245};
    auto dark_catacomb = assembleStructure<>(cat);

    StructureDef twi;
    twi.name = "Twilight Cavern";
    twi.parent = celestial_plane;
    twi.roomRange = {7300, 7500};
    auto twilight_cavern = assembleStructure<>(twi);

    StructureDef helldef;
    helldef.name = "Hell";
    helldef.parent = celestial_plane;
    auto hell = assembleStructure<>(helldef);

    StructureDef hfields;
    hfields.name = "Hell Fields";
    hfields.parent = hell;
    hfields.roomRange = {6200, 6300};

    StructureDef hsands;
    hsands.name = "Sands of Time";
    hsands.parent = hell;
    hsands.roomRange = {6300, 6349};
    auto sands_of_time = assembleStructure<>(hsands);

    StructureDef hchaotic;
    hchaotic.name = "Chaotic Spiral";
    hchaotic.parent = hell;
    hchaotic.roomRange = {6349, 6400};
    auto chaotic_spiral = assembleStructure<>(hchaotic);

    StructureDef hfirecity;
    hfirecity.name = "Hellfire City";
    hfirecity.parent = hell;
    hfirecity.roomRange = {6400, 6530};
    hfirecity.roomIDs = {6568, 6569, 6600, 6699};
    auto hellfire_city = assembleStructure<>(hfirecity);

    StructureDef fbagdojo;
    fbagdojo.name = "Flaming Bag Dojo";
    fbagdojo.parent = hellfire_city;
    fbagdojo.roomRange = {6530, 6568};
    auto flaming_bag_dojo = assembleStructure<>(fbagdojo);

    StructureDef etrailgrave;
    etrailgrave.name = "Entrail Graveyard";
    etrailgrave.parent = hellfire_city;
    etrailgrave.roomRange = {6601, 6689};
    auto entrail_graveyard = assembleStructure<>(etrailgrave);

    StructureDef psihnon;
    psihnon.name = "Planet Sihnon";
    psihnon.parent = space;
    psihnon.roomRange = {3600, 3700};
    auto planet_sihnon = assembleStructure<CelestialBody>(psihnon);

    StructureDef majdef;
    majdef.name = "Majinton";
    majdef.parent = planet_sihnon;
    majdef.roomRange = {3700, 3797};
    auto majinton = assembleStructure<Dimension>(majdef);

    StructureDef wistower;
    wistower.name = "Wisdom Tower";
    wistower.parent = planet_namek;
    wistower.roomRange = {9600, 9667};
    auto wisdom_tower = assembleStructure<>(wistower);

    StructureDef machia;
    machia.name = "Machiavilla";
    machia.parent = planet_konack;
    machia.roomRange = {12743, 12798};
    auto machiavilla = assembleStructure<>(machia);

    StructureDef monbal;
    monbal.name = "Monastery of Balance";
    monbal.parent = planet_konack;
    monbal.roomRange = {9500, 9599};

    StructureDef futschool;
    futschool.name = "Future School";
    futschool.parent = xenoverse;
    futschool.roomRange = {15938, 16000};
    auto future_school = assembleStructure<Dimension>(futschool);

    StructureDef udfhq;
    udfhq.name = "UDF Headquarters";
    udfhq.parent = space;
    udfhq.roomRange = {18000, 18059};
    auto udf_headquarters = assembleStructure<CelestialBody>(udfhq);
    {
        auto &celbod = registry.get_or_emplace<CelestialBody>(udf_headquarters);
        celbod.type = celtype::STATION;
    }

    StructureDef hspire;
    hspire.name = "The Haven Spire";
    hspire.parent = space;
    hspire.roomRange = {18300, 18341};
    auto haven_spire = assembleStructure<CelestialBody>(hspire);
    {
        auto &celbod = registry.get_or_emplace<CelestialBody>(haven_spire);
        celbod.type = celtype::STATION;
    }

    StructureDef knoit;
    knoit.name = "Kame no Itto";
    knoit.parent = space;
    knoit.roomRange = {18400, 18460};
    auto kame_no_itto = assembleStructure<CelestialBody>(knoit);
    {
        auto &celbod = registry.get_or_emplace<CelestialBody>(kame_no_itto);
        celbod.type = celtype::STATION;
    }

    StructureDef southgal;
    southgal.name = "South Galaxy";
    southgal.parent = mortal_plane;
    southgal.roomIDs = {64300, 64399};
    auto south_galaxy = assembleStructure<>(southgal);

    StructureDef shatplan;
    shatplan.name = "Shattered Planet";
    shatplan.parent = south_galaxy;
    shatplan.roomRange = {64301, 64399};
    auto shattered_planet = assembleStructure<CelestialBody>(shatplan);
    {
        auto &celbod = registry.get_or_emplace<CelestialBody>(shattered_planet);
        celbod.type = celtype::PLANET;
    }

    StructureDef wzdef;
    wzdef.name = "War Zone";
    wzdef.parent = xenoverse;
    wzdef.roomRange = {17700, 17703};
    auto war_zone = assembleStructure<Dimension>(wzdef);

    StructureDef corlight;
    corlight.name = "Corridor of Light";
    corlight.parent = war_zone;
    corlight.roomRange = {17703, 17723};
    auto corridor_of_light = assembleStructure<>(corlight);

    StructureDef cordark;
    cordark.name = "Corridor of Darkness";
    cordark.parent = war_zone;
    cordark.roomRange = {17723, 17743};
    auto corridor_of_darkness = assembleStructure<>(cordark);

    StructureDef soisland;
    soisland.name = "South Ocean Island";
    soisland.parent = planet_earth;
    soisland.roomRange = {6700, 6758};
    auto south_ocean_island = assembleStructure<>(soisland);

    StructureDef hhouse;
    hhouse.name = "Haunted House";
    hhouse.parent = xenoverse;
    hhouse.roomRange = {18600, 18693};
    auto haunted_house = assembleStructure<Dimension>(hhouse);

    StructureDef roc;
    roc.name = "Random Occurences, WTF?";
    roc.parent = xenoverse;
    roc.roomRange = {18700, 18776};
    auto random_occurences = assembleStructure<Dimension>(roc);

    StructureDef galstrong;
    galstrong.name = "Galaxy's Strongest Tournament";
    galstrong.parent = space;
    galstrong.roomRange = {17875, 17894};
    auto galaxy_strongest_tournament = assembleStructure<>(galstrong);

    StructureDef arwater;
    arwater.name = "Arena - Water";
    arwater.parent = galaxy_strongest_tournament;
    arwater.roomRange = {17800, 17825};
    auto arena_water = assembleStructure<>(arwater);

    StructureDef arring;
    arring.name = "Arena - The Ring";
    arring.parent = galaxy_strongest_tournament;
    arring.roomRange = {17825, 17850};
    auto arena_ring = assembleStructure<>(arring);

    StructureDef arsky;
    arsky.name = "Arena - In the Sky";
    arsky.parent = galaxy_strongest_tournament;
    arsky.roomRange = {17850, 17875};
    auto arena_sky = assembleStructure<>(arsky);

    auto crunch_ship = [&](old_ship_data &data, bool g) {

        StructureDef sdata;
        sdata.name = data.name;
        sdata.roomIDs = data.vnums;
        sdata.global = g;
        auto ship = assembleStructure<Vehicle>(sdata);
        auto &rooms = registry.get<Area>(ship);
        if(data.hatch_room) {
            auto find = rooms.data.find(data.hatch_room.value());
            if(find == rooms.data.end()) {
                std::cout << "Hatch room not found for ship " << data.name << std::endl;
                return ship;
            }
            auto hroom = rooms.data[data.hatch_room.value()];
            auto &rmflags = registry.get_or_emplace<RoomFlags>(hroom);
            rmflags.data[rflags::HATCH] = true;
            rmflags.data[rflags::CONTROLS] = true;
        }

        if(data.ship_obj) {
            auto &obj = obj_proto[data.ship_obj.value()];
            //setName(ship, obj.name);
            setShortDescription(ship, cleanString(obj.short_description));
            setRoomDescription(ship, cleanString(obj.room_description));
        }

        return ship;
    };

    for(auto &sd : gships) {
        crunch_ship(sd, true);
    }

    for(auto &sd : customs) {
        crunch_ship(sd, true);
    }

    auto clear_rooms = [&](RoomId start, RoomId finish) {
        for(RoomId r = start; r < finish; r++) {
            unknowns.erase(r);
        }
    };

    clear_rooms(19800, 19900);
    clear_rooms(46000, 46150);
    clear_rooms(18800, 19199);

    // A very luxurious player custom home
    StructureDef dunnoHouse;
    dunnoHouse.name = "Dunno's House";
    dunnoHouse.parent = xenoverse;
    dunnoHouse.roomIDs = {19009, 19010, 19011, 19012, 19013, 19014, 19015, 19016, 19017, 19018,
                          19019, 19020, 19021, 19022, 19023};
    auto dunno_house = assembleStructure<>(dunnoHouse);

    // This looks like an unused old player home, seems like it's attached to Cherry Blossom Mountain?
    StructureDef mountainFortress;
    mountainFortress.name = "Mountaintop Fortress";
    mountainFortress.parent = xenoverse;
    mountainFortress.roomIDs = {19025, 19026, 19027, 19028, 19029, 19030, 19031, 19032,
                                19033, 19034, 19035, 19036, 19037, 19038};
    auto mountain_fortress = assembleStructure<>(mountainFortress);

    StructureDef misc;
    misc.name = "Miscellaneous";
    misc.roomIDs = unknowns;
    auto misc_area = assembleStructure<>(misc);


    // Now that all the areas are created, we need to scan through them all...
    // It'd be great to set their Destination component to properly target the
    // right ObjectId. In addition, those which target legacy space grid need
    // to point at space and the right coordinates.
    auto view = registry.view<Exits>();
    for (auto ent : view) {
        auto &ex = view.get<Exits>(ent);
        std::vector<dir::DirectionId> badDirs;
        for(auto &[dir, e] : ex.data) {
            auto destId = e.x;
            auto find = legacyRooms.find(destId);
            if(find == legacyRooms.end()) {
                logger->info("Room has an exit to {}, which is not found in the legacy rooms list.", destId);
                badDirs.emplace_back(dir);
                continue;
            } else {
                e.data = find->second.data;
                e.locationType = LocationType::Area;
            }
        }
        for(auto &bd : badDirs) {
            ex.data.erase(bd);
        }
    }

}


void dump_db() {
    std::cout << "Beginning migration..." << std::endl;
    setupGame();
    logger->info("Setup Game successful.");

    // start a transaction.
    auto transaction = SQLite::Transaction(*db);

    // First, convert zones.
    logger->info("Migrating Zones...");
    dump_zones();

    // Then scripts..
    logger->info("Migrating Scripts...");
    dump_scripts();

    // dump prototypes...
    logger->info("Migrating Item Prototypes...");
    dump_items();
    logger->info("Migrating NPC Prototypes...");
    dump_npc();

    // Now we can dump rooms..
    logger->info("Building structures and migrating rooms...");
    migrate_objects();
    logger->info("Migrating accounts and player characters...");
    dump_accounts();
    logger->info("Setting everything dirty...");
    gameIsLoading = false;
    for(auto &o : objects) {
        if(registry.valid(o.second)) setDirty(o.second);
    }
    logger->info("Saving all dirty objects to database...");
    processDirty();
    saveLegacySpace();
    logger->info("Saving Scripts...");
    saveScripts();
    logger->info("Saving Zones...");
    saveZones();
    transaction.commit();
    logger->info("Transaction committed successfully.");

}

int main(int argc, char **argv)
{
    int pos = 1;
    const char *dir;
    // let's time the boot loader. gimme a timestamp, high resolution.
    auto start = std::chrono::high_resolution_clock::now();

#ifdef MEMORY_DEBUG
    zmalloc_init();
#endif

#if CIRCLE_GNU_LIBC_MEMORY_TRACK
    mtrace();	/* This must come before any use of malloc(). */
#endif

    /****************************************************************************/
    /** Load the game configuration.                                           **/
    /** We must load BEFORE we use any of the constants stored in constants.c. **/
    /** Otherwise, there will be no variables set to set the rest of the vars  **/
    /** to, which will mean trouble --> Mythran                                **/
    /****************************************************************************/
    CONFIG_CONFFILE = nullptr;
    while ((pos < argc) && (*(argv[pos]) == '-')) {
        if (*(argv[pos] + 1) == 'f') {
            if (*(argv[pos] + 2))
                CONFIG_CONFFILE = argv[pos] + 2;
            else if (++pos < argc)
                CONFIG_CONFFILE = argv[pos];
            else {
                puts("SYSERR: File name to read from expected after option -f.");
                exit(1);
            }
        }
        pos++;
    }
    pos = 1;

    if (!CONFIG_CONFFILE)
        CONFIG_CONFFILE = strdup(CONFIG_FILE);

    load_config();

    port = CONFIG_DFLT_PORT;
    dir = CONFIG_DFLT_DIR;

    while ((pos < argc) && (*(argv[pos]) == '-')) {
        switch (*(argv[pos] + 1)) {
            case 'f':
                if (! *(argv[pos] + 2))
                    ++pos;
                break;
            case 'o':
                if (*(argv[pos] + 2))
                    CONFIG_LOGNAME = argv[pos] + 2;
                else if (++pos < argc)
                    CONFIG_LOGNAME = argv[pos];
                else {
                    puts("SYSERR: File name to log to expected after option -o.");
                    exit(1);
                }
                break;
            case 'C': /* -C<socket number> - recover from copyover, this is the control socket */
                fCopyOver = true;
                mother_desc = atoi(argv[pos]+2);
                break;
            case 'd':
                if (*(argv[pos] + 2))
                    dir = argv[pos] + 2;
                else if (++pos < argc)
                    dir = argv[pos];
                else {
                    puts("SYSERR: Directory arg expected after option -d.");
                    exit(1);
                }
                break;
            case 'm':
                mini_mud = 1;
                no_rent_check = 1;
                puts("Running in minimized mode & with no rent check.");
                break;
            case 'c':
                scheck = 1;
                puts("Syntax check mode enabled.");
                break;
            case 'q':
                no_rent_check = 1;
                puts("Quick boot mode -- rent check supressed.");
                break;
            case 'r':
                circle_restrict = 1;
                puts("Restricting game -- no new players allowed.");
                break;
            case 's':
                no_specials = 1;
                puts("Suppressing assignment of special routines.");
                break;
            case 'x':
                xap_objs = 1;
                log("Loading player objects from secondary (ascii) files.");
                break;
            case 'h':
                /* From: Anil Mahajan <amahajan@proxicom.com> */
                printf("Usage: %s [-c] [-m] [-x] [-q] [-r] [-s] [-d pathname] [port #]\n"
                       "  -c             Enable syntax check mode.\n"
                       "  -d <directory> Specify library directory (defaults to 'lib').\n"
                       "  -f<file>       Use <file> for configuration.\n"
                       "  -h             Print this command line argument help.\n"
                       "  -m             Start in mini-MUD mode.\n"
                       "  -o <file>      Write log to <file> instead of stderr.\n"
                       "  -q             Quick boot (doesn't scan rent for object limits)\n"
                       "  -r             Restrict MUD -- no new players allowed.\n"
                       "  -s             Suppress special procedure assignments.\n"
                       " Note:         These arguments are 'CaSe SeNsItIvE!!!'\n"
                       "  -x             Load using secondary (ascii) files.\n",
                       argv[0]
                );
                exit(0);
            default:
                printf("SYSERR: Unknown option -%c in argument string.\n", *(argv[pos] + 1));
                break;
        }
        pos++;
    }

    if (pos < argc) {
        if (!isdigit(*argv[pos])) {
            printf("Usage: %s [-c] [-m] [-q] [-r] [-s] [-d pathname] [port #]\n", argv[0]);
            exit(1);
        } else if ((port = atoi(argv[pos])) <= 1024) {
            printf("SYSERR: Illegal port number %d.\n", port);
            exit(1);
        }
    }

    /* All arguments have been parsed, try to open log file. */
    setup_log(CONFIG_LOGNAME, STDERR_FILENO);

    /*
     * Moved here to distinguish command line options and to show up
     * in the log if stderr is redirected to a file.
     */
    log("Using %s for configuration.", CONFIG_CONFFILE);
    log("%s", circlemud_version);
    log("%s", oasisolc_version);
    log("%s", DG_SCRIPT_VERSION);
    log("%s", ascii_pfiles_version);
    log("%s", CWG_VERSION);
    xap_objs = 1;
    if (chdir(dir) < 0) {
        perror("SYSERR: Fatal error changing to data directory");
        exit(1);
    }
    log("Using %s as data directory.", dir);

    init_lookup_table();
    event_init();
    boot_db();
    log("Database fully booted!");

    FILE *mapfile;
    int rowcounter, colcounter;
    int vnum_read;
    log("Loading Space Map. ");
    //Load the map vnums from a file into an array
    mapfile = fopen("../lib/surface.map", "r");

    for (rowcounter = 0; rowcounter <= MAP_ROWS; rowcounter++) {
        for (colcounter = 0; colcounter <= MAP_COLS; colcounter++) {
            fscanf(mapfile, "%d", &vnum_read);
            mapnums[rowcounter][colcounter] = real_room(vnum_read);
        }
    }
    fclose(mapfile);

    /* Load the toplist */
    topLoad();
    // time! how long did it take?
    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> elapsed_seconds = end-start;
    log("Boot took %f seconds.", elapsed_seconds.count());

    // BEGIN DUMP SEQUENCE
    log("Dumping db!");
    dump_db();
    log("Dumping db complete!");


    return 0;
}
