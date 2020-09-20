getFeatures <- function(
# accepts a GFF file name
# returns a list of GRangesLists corresponding to genomic features
  gff, # path to GFF
  prefix="KY2019:",
  tssflank=c(1107,107), # bp width around TSS to use as promoter
  tsswindow=10000, # bp width upstream of TSS
  ttswindow=10000, # bp width downstream of TTS
  ...
){
  # library for working with bed files
	#   require(GenomicRanges)
  # library for extracting genomic features from GFF
	#   require(GenomicFeatures)
  # read data
  gff <- import(gff)
  # add fields for output as bed files
  gff$score <- 0
  gff$name <- gff$Parent

  # extract features by type
  res <- lapply(
	c('five_prime_UTR','CDS','three_prime_UTR'),
	function(x) {
		y <- gff[mcols(gff)$type==x]
		return(setNames(y,mcols(y)$Parent))
	}
  )
  names(res) <- c('five_prime_UTR','CDS','three_prime_UTR')

  # write output
  #   lapply(names(res), function(x) dir.export(res[[x]],x,...))

  # extract all features for each transcript
  gene <- gff[mcols(gff)$type%in%c("CDS",'five_prime_UTR','three_prime_UTR')]
  gene <- split(gene,unlist(mcols(gene)$Parent))
  genebody <- unlist(range(gene))
  #   fn <- function(x) {
  #         gaps(x,min(start(x)),max(end(x)))
  #   }
  #   gaps(reduce(gene[1:1000]),start(genebody)[1:1000],end(genebody)[1:1000])
  #   intron <- mapply(gaps,gene,start(genebody),end(genebody),SIMPLIFY=F)

  downstream <- flank(genebody,ttswindow,start=F)
  #   start(downstream)[start(downstream)<1] <- 1

  #   res <- lapply(res,function(x) split(x,unlist(mcols(x)$Parent)))

  txdb <- makeTxDbFromGRanges(gff)
  
  introns <- unlist(intronsByTranscript(txdb,T))
  names(introns) <- paste0(prefix,names(introns))
  #   names(introns) <- sub('\\.v.*','',names(introns))
  
  tss <- promoters(genebody,tssflank[1],tssflank[2])

  upstream <- flank(tss,tsswindow-tssflank[1])

  res2 <- list(upstream=upstream,promoter=tss,intron=introns,downstream=downstream)
  res2 <- append(res2,res)
  lapply(names(res2), function(x) dir.export(res2[[x]],x,...))

  #   res2 <- lapply(res2, function(x) setNames(x,sub('\\.v.*','',names(x))))
  res2 <- lapply(res2, function(x) split(x,sub('\\.v.*','',names(x))))

  #   res <- lapply(res, function(x) setNames(x,sub('\\.v.*','',names(x))))

  return(res2)
}

# finds all overlaps between a set of genomic features and a set of peaks
# then returns a table o gene-to-peak associations
getOverlaps <- function(feat,peaks){
	tmp <- findOverlaps(peaks,feat)
	res <- data.frame(
		PeakID=peaks$name[from(tmp)],
		GeneID=names(feat)[to(tmp)],
		stringsAsFactors=F
	)
	res <- res[!duplicated(res),]
	return(res)
}
