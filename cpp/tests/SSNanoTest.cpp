#include "StrideSearchUtilities.h"
#include "StrideSearchSectorListBase.h"
#include "StrideSearchDataBase.h"
#include <vector>
#include <cmath>
#include <memory>
#include <iostream>
#include <sstream>
#include <cassert>
#include <limits>
#include <algorithm>
#include <numeric>
#include "StrideSearchNanoflannAdaptor.h"

using namespace StrideSearch;
bool compareSectors(std::vector<std::unique_ptr<Sector>>& a, std::vector<std::unique_ptr<Sector>>& b);

int main(int argc, char* argv[]) {
  print_copyright();
  
  std::string llGridFile = StrideSearch_TEST_DATA_DIR;
  std::string conusGridFile = StrideSearch_TEST_DATA_DIR;
  std::string csGridFile = StrideSearch_TEST_DATA_DIR;
  
  llGridFile += "/";
  conusGridFile += "/";
  csGridFile += "/";
  
  llGridFile += "sresa1b_ncar_ccsm3-example.nc";
  conusGridFile += "conusx4v1.g";
  csGridFile += "uniform_16_quad4.g";
  
  std::shared_ptr<StrideSearchData> llData(new StrideSearchData(llGridFile));
  std::shared_ptr<StrideSearchData> conusData(new StrideSearchData(conusGridFile));
  std::shared_ptr<StrideSearchData> csData(new StrideSearchData(csGridFile));
  
  llData->initDimensions();
  conusData->initDimensions();
  csData->initDimensions();

  const scalar_type sb = -90.0;
  const scalar_type nb = 90.0;
  const scalar_type wb = 0.0;
  const scalar_type eb = 360.0;

  const scalar_type sector_radius = 3000.0;

  SectorList llSectorsN(sb, nb, wb, eb, sector_radius);
  SectorList llSectors(sb, nb, wb, eb, sector_radius);
  SectorList conusSectorsN(sb, nb, wb, eb, sector_radius);
  SectorList conusSectors(sb, nb, wb, eb, sector_radius);
  SectorList csSectorsN(sb, nb, wb, eb, sector_radius);
  SectorList csSectors(sb, nb, wb, eb, sector_radius);

  csSectorsN.linkSectorsToDataWNano(csData);
  std::cout << csSectorsN.infoString() << std::endl;
  csSectors.linkSectorsToDataWOutNano(csData);
  std::cout << csSectors.infoString() << std::endl;

  compareSectors(csSectorsN.sectors,csSectors.sectors);
  //compareSectors(csSectors.sectors,csSectorsN.sectors);
}

bool compareVect(std::vector<scalar_type> a, std::vector<scalar_type> b) {
  if(a.size() != b.size()) {
    std::cout<<"coords contained of the two vectors do not have same size."<<std::endl;
    return false;
  }
  for(int i = 0; i < a.size(); i++) {
    for(int j = 0; j < b.size(); j++) {
      if(a[i] == b[j]) {
	b.erase(b.begin() + j);
	//std::cout<<a[i]<<std::endl;
	break;
      }
    }
  }
  //std::cout<<b.size()<<std::endl;
  return b.size() == 0;
}

bool compareSectors(std::vector<std::unique_ptr<Sector>>& nano, std::vector<std::unique_ptr<Sector>>& noNano) {
  std::vector<scalar_type> a;
  std::vector<scalar_type> b;
  if(nano.size() != noNano.size()) {
    std::cout<<"vectors are not equal."<<std::endl;
    return false; 
  }
  for(int i = 0; i < nano.size(); i++) {
    for(int j = 0; j < nano[i]->data_indices.size(); j++) {
      //Make method to compare data_indices.at(j) instead of below method.
      for(int k = 0; k < nano[i]->data_indices.at(j).size(); k++) {
	a.push_back(nano[i]->data_indices.at(j)[k]);
	b.push_back(noNano[i]->data_indices.at(j)[k]);
      }
    }
  }
  if(!compareVect(a,b)) {
    std::cout<<"vectors are not equal."<<std::endl;
    return false;
  }
  else {
    std::cout<<"vectors are equal."<<std::endl;
    return true;
  }
}

void SectorList::linkSectorsToDataWNano(const std::shared_ptr<StrideSearchData> data_ptr) {

  std::cout << "Running Stride with nano" << std::endl;
  typedef nanoflann::KDTreeSingleIndexAdaptor<SphereDistAdaptor<scalar_type, NanoflannAdaptor>, NanoflannAdaptor, 3, index_type> tree_type;
  NanoflannAdaptor adaptor(data_ptr);
  const int max_leaf_size = 10;
  nanoflann::KDTreeSingleIndexAdaptorParams params(max_leaf_size);
  tree_type search_tree(3, adaptor, params);
  search_tree.buildIndex();

  for (index_type secInd = 0; secInd < nSectors(); ++secInd){
    std::vector<std::pair<index_type, scalar_type>> return_matches;
    // radius search from sector center with sector radius                                                                                                                                              
    scalar_type xyz[3];
    llToXYZ(xyz[0], xyz[1], xyz[2], sectors[secInd]->centerLat, sectors[secInd]->centerLon);

    nanoflann::SearchParams params;

    const index_type nMatches = search_tree.radiusSearch(&xyz[0], sectors[secInd]->radius * sectors[secInd]->radius, return_matches, params);
    if (data_ptr->layout1d()) {
      for (index_type i = 0; i < nMatches; ++i) {
	std::vector<index_type> llind = {return_matches[i].first};
	sectors[secInd]->data_indices.push_back(llind);
      }
    }
    else if (data_ptr->layout2d()) {
      for (index_type i = 0; i < nMatches; ++i) {
	const std::pair<index_type, index_type> llpair = data_ptr->get2dIndex(return_matches[i].first);
	const std::vector<index_type> llind = {llpair.first, llpair.second};
	sectors[secInd]->data_indices.push_back(llind);
      }
    }
  }
}

void SectorList::linkSectorsToDataWOutNano(const std::shared_ptr<StrideSearchData> data_ptr) {
  if (data_ptr->layout1d()) {                                                                          
    for (index_type sec_i = 0; sec_i < sectors.size(); ++sec_i) {
      for (index_type i = 0; i < data_ptr->lats.size(); ++ i) {
	const scalar_type dist = sphereDistance(sectors[sec_i]->centerLat, sectors[sec_i]->centerLon,
						data_ptr->lats[i], data_ptr->lons[i]);
	if ( dist <= sectors[sec_i]->radius) {
	  sectors[sec_i]->data_coords.push_back(ll_coord_type(data_ptr->lats[i], data_ptr->lons[i]));
	  const std::vector<index_type> ind = {i};
	  sectors[sec_i]->data_indices.push_back(ind);
	}
      }
    }
  }
  if (data_ptr->layout2d()) {                                                                    
    for (index_type sec_i = 0; sec_i < sectors.size(); ++sec_i) {
      for (index_type i = 0; i < data_ptr->lats.size(); ++i) {
	for (index_type j = 0; j < data_ptr->lons.size(); ++j) {
	  const scalar_type dist = sphereDistance(sectors[sec_i]->centerLat, sectors[sec_i]->centerLon,
						  data_ptr->lats[i], data_ptr->lons[j]);
	  if ( dist <= sectors[sec_i]->radius ) {
	    sectors[sec_i]->data_coords.push_back(ll_coord_type(data_ptr->lats[i], data_ptr->lons[j]));
	    const std::vector<index_type> ind = {i, j};
	    sectors[sec_i]->data_indices.push_back(ind);
	  }
	}
      }
    }
  }
}

