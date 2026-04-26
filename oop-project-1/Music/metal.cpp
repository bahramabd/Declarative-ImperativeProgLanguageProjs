#include "kpop.h"
#include "metal.h"
#include "jazz.h"
#include "rock.h"


int MetalBand::play(MusicBand *other)
{
    int score;
    KPopBand* n = dynamic_cast<KPopBand*>(other);
    if (n != NULL){
        score = (this->get_fan_count() + 0.1 * this->get_talent() * this->get_energy()) * 0.5;
    }
    
    MetalBand* n1 = dynamic_cast<MetalBand*>(other);
    if (n1 != NULL){
        score = (this->get_fan_count() + 0.1 * this->get_talent() * this->get_energy()) * 1;
    }
    JazzBand* n2 = dynamic_cast<JazzBand*>(other);
    if (n2 != NULL){
        score = (this->get_fan_count() + 0.1 * this->get_talent() * this->get_energy()) * 1.1;
    }
    RockBand* n3 = dynamic_cast<RockBand*>(other);
    if (n3 != NULL){
        score = (this->get_fan_count() + 0.1 * this->get_talent() * this->get_energy()) * 1.5;
    }
    this->set_energy(this->get_energy() - (this->get_energy() * 0.16));
    return score;
}

void MetalBand::rehearse(void) 
{
    this->set_energy(this->get_energy() - (this->get_energy() * 0.5 * 0.16));
    this->set_talent(this->get_talent() - 5);
}