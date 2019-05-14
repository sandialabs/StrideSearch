#include "SSData.hpp"
#include "nanoflann.hpp"
#include "SSUtilities.hpp"
#include <netcdf>

namespace StrideSearch {

template <typename ReaderType>
SSData<ReaderType>::SSData(const std::string& fname) {
    reader = std::unique_ptr<NCReader>(new ReaderType(fname));
    loadTimeVariable();        
    tree = KDTree(reader.get());
}

template <typename ReaderType>
void SSData<ReaderType>::linkSectorsToData(SectorSet<Layout>& sec_set) {
    for (Index i=0; i<sec_set.nSectors(); ++i) {
        const Real radius = sec_set.sectors[i]->radius;
        Real cx, cy, cz;
        llToXYZ(cx, cy, cz, sec_set.sectors[i]->lat, sec_set.sectors[i]->lon);
        const Real sec_center[3] = {cx,cy,cz};
        std::vector<std::pair<Index,Real>> indices_and_dists;
        nf::RadiusResultSet<Real,Index> resultSet(radius, indices_and_dists);
        tree.index->findNeighbors(resultSet, sec_center, nf::SearchParams());
        for (Index j=0; j<indices_and_dists.size(); ++j) {
            const Index pts_index = indices_and_dists[j].first;
            sec_set.sectors[i]->indices.push_back(reader->getDataIndFromTreeInd(pts_index));
            sec_set.sectors[i]->lats.push_back(reader->getLat(pts_index));
            sec_set.sectors[i]->lons.push_back(reader->getLon(pts_index));
        }
    }
}

template <typename ReaderType>
void SSData<ReaderType>::loadSectorWorkingData(Sector<Layout>& sec, const Index& time_ind,
    const Index& lev_ind) const {
    /// Step 1: loop over each Workspace in Sector
    for (int i=0; i<sec.workspaces.size(); ++i) {
        /// Step 2: loop over each variable in Workspace
        for (auto& elem : workspaces[i].data) {
            netCDF::NcVar ncv(*(reader->ncfile).getVar(elem.first));
            const int n_var_dim = ncv.getDimCount();
            /// Step 3: Loop over each index in the sector, load variable at that index
            switch (n_var_dim) {
                case (Layout::value): {
                    std::vector<size_t> get_index(Layout::value+1,0);
                    get_index[0] = time_ind;
                    for (Index j=0; j<sec.nDataPoints(); ++j) {
                        const typename Layout::horiz_index_type hind = sec.indices[j];
                        for (int k=0; k<Layout::value; ++k) {
                            get_index[k+1] = hind[k];
                        }
                        ncv.getVar(get_index, &elem.second[i]);
                    }
                    break;
                }
                case (Layout::value+1): {
                    std::vector<size_t> get_index(Layout::value+2,0);
                    get_index[0] = time_ind;
                    get_index[1] = lev_ind;
                    for (Index j=0; j<sec.nDataPoints(); ++j) {
                        const typename Layout::horiz_index_type hind = sec.indices[j];
                        for (int k=0; k<Layout::value; ++k) {
                            get_index[k+2] = hind[k];   
                        }
                        ncv.getVar(get_index, &elem.second[i]);
                    }
                    break;
                }
            }
            default: {
                throw std::runtime_error("SSData::loadSectorWorkingData error: invalid n_var_dim");
            }
        }
    }
}

}
