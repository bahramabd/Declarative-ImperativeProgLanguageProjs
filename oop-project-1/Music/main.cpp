#include "kpop.h"
#include "jazz.h"
#include "metal.h"
#include "rock.h"
#include "music_band.h"
#include "tournament.h"
#include "tournament_round.h"


int main()
{
    
    JazzBand band1("jazz", 50, 100, 25);
    RockBand band2("rock", 50, 100, 25);
    MetalBand band3("metal", 50, 100, 25);
    KPopBand band4("kpop", 50, 100, 25);
    std::vector<MusicBand*> ba;
    std::vector<MusicBand*> b;
    b.push_back(&band1);b.push_back(&band2);b.push_back(&band3);
    ba.push_back(&band1);ba.push_back(&band2);ba.push_back(&band3);ba.push_back(&band4);
    TournamentRound a(ba);
    TournamentRound iki(b);
    //std::cout<<a<<std::endl;
    //std::cout<<a.get_next_round()<<std::endl;
    iki=std::move(a.get_next_round());
    std::cout<<a<<std::endl;
    //std::cout<<a.get_next_round()<<std::endl;
    
    
    return 0;
}