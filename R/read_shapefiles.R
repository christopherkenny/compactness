#' Read Shapes into compactnessShapefiles
#' 
#' This script takes the name a .shp file and breaks it apart into its metadata and coordinates.
#'
#' @param shp The filename of a shp file containing district polygons or an sf dataframe.
#' @param namecol The ID of the column with the district name in it
#' @param verbose Default TRUE. Inherited from comapactness_wrapper(). 
#' 
#' @return A list of three: the metadata in n x p where n is the number of districts in the shapefile and p is the number of covariates; a list of lists of n where each sublist is a lat-long matrix, and a sublist of length >1 indicates a noncontiguous district; and the name column
#' 
#' @export
#' @examples
#' read_shapefiles("CnclDist_July2012.shp")
read_shapefiles = function(shp, namecol, verbose=TRUE){ # namecol specifies the id of the column with the district name in it
  
  
  if(!inherits(shp, 'sf')){
    if(!file.exists(shp)) {
      stop("shp argument must be a filepath!")
    }
    
    dists <- sf::st_read(shp, quiet = TRUE)
  } else {
    dists <- shp
  }
  
  l <- nrow(dists)
  metadata <- dists #%>% sf::st_drop_geometry()
  
  if(is.numeric(namecol)) {
    if(namecol > ncol(metadata)) {
      stop('`namecol` is numeric, but too few non-geometry columns in shp.')
    }
    test <- metadata[,namecol]
  } else if(is.character(namecol)){
    if(!namecol %in% names(namecol)) {
      stop('`namecol` is character, but not a name in shp.')
    }
    test <- metadata[[namecol]]
  }
  if(verbose) print("Successfully located the identifier column. ")
  
  proj <- dists %>% st_transform(4326)
  if(verbose) print(paste("Loaded coordinates for ", l, " districts.", sep=""))
  
  #areas = sapply(slot(proj, "polygons"), slot, "area") * 1000000
  #metadata$district_area = areas
  # I need area calculated in here. And it needs to identify hole polygons.
  
  coords <- lapply(1:nrow(proj), FUN=function(x) get_multi_coord(proj, x))
  
  if(verbose) print(paste("Successfully extracted coordinates from ", l, " districts.", sep=""))   

  out = structure(list(metadata, coords, namecol), class="compactnessShapefile")
  
  return(out)
}
