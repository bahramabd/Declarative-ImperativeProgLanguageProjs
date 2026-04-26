#include "tournament_round.h"

// TournamentRound functions goes here

TournamentRound::TournamentRound() { 
   bands.clear();
    
}
TournamentRound::TournamentRound(std::list<MusicBand*>_bands) { 
   bands=_bands;
}
TournamentRound::TournamentRound(std::vector<MusicBand*>_bands) {
    bands.clear();
    for (int i=0;i<_bands.size();i++)  bands.push_back(_bands[i]);
}
std::size_t TournamentRound::size() { return bands.size(); }
//TournamentRound::TournamentRound(TournamentRound& other) { }
//TournamentRound::TournamentRound(TournamentRound&& other) { }
TournamentRound& TournamentRound::operator=(TournamentRound&& other) { 
   // bands=other.bands;
    //other.bands.clear();
    
    return *this; 
    
}
TournamentRound& TournamentRound::get_next_round() { 
    std::list<MusicBand*> ba;
    std::list<MusicBand*> bands1=bands;
    int score1; int score2;
    while (bands1.size() > 1) {
        if (bands1.front()->get_energy()<0){
            int fa=((score2-score1)>=(bands1.front())->get_fan_count()) ? (bands1.front())->get_fan_count() : (score2-score1);
            (bands1.back())->set_fan_count((bands1.back())->get_fan_count() + fa);
            bands1.front()->set_fan_count(bands1.front()->get_fan_count()-fa);
            ba.push_back(bands1.back());
        }
        else if (bands1.back()->get_energy()<0){
            int fa=((score1-score2)>=(bands1.back())->get_fan_count()) ? (bands1.back())->get_fan_count() : (score1-score2);
            (bands1.front())->set_fan_count((bands1.front())->get_fan_count() + fa);
            bands1.back()->set_fan_count(bands1.back()->get_fan_count()-fa);
            ba.push_back(bands1.front());
        }
        else{
           // std::cout<<bands1.front()->get_energy()<<" "<<bands1.front()->get_talent()<<" "<<bands1.front()->get_fan_count()<<" "<<std::endl;
        score1 = (bands1.front())->play(bands1.back());
        score2 = (bands1.back())->play(bands1.front());
        //std::cout<<score1<<" and "<<score2<<std::endl;
        if (score1 > score2){
            int fa=((score1-score2)>=(bands1.back())->get_fan_count()) ? (bands1.back())->get_fan_count() : (score1-score2);
            (bands1.front())->set_fan_count((bands1.front())->get_fan_count() + fa);
            bands1.back()->set_fan_count(bands1.back()->get_fan_count()-fa);
            ba.push_back(bands1.front());
        }  
        else if (score2>score1){
            int fa=((score2-score1)>=(bands1.front())->get_fan_count()) ? (bands1.front())->get_fan_count() : (score2-score1);
            (bands1.back())->set_fan_count((bands1.back())->get_fan_count() + fa);
            bands1.front()->set_fan_count(bands1.front()->get_fan_count()-fa);
            ba.push_back(bands1.back());
        }
        else{
            ba.push_back(bands1.front());
        }}
    bands1.remove(bands1.front());
       bands1.remove(bands1.back());
    }
    if (bands1.size()==1){
        ba.push_back(bands1.front());
        bands1.remove(bands1.front());
    }
    
    bands=ba;
    
    return *this;
}

std::ostream& operator<< (std::ostream &os, TournamentRound &other) {
    std::list<MusicBand*> bands1=other.bands;
    while (bands1.size()>0){
        if (bands1.size()==1){
            os<<bands1.front()->get_name();
            bands1.remove(bands1.front());
            return os;
        }
        
        os << bands1.front()->get_name() << "\t";
        bands1.remove(bands1.front());
    }
    return os; 
}
















