## I decided to experiment with Reference Classes to see if I could
## use Object Oriented methodologies as a better framework for implementing
## Coursera projects. However, there seems to be some issues with storing
## large data.frame objects within the parent environment.
## So for now, this is merely a utility class for getting and staging the
## necessary remote zip file and extracting them locally.
## I have commented the releveant methods below.
ParticulateMatter <- setRefClass(
    Class="ParticulateMatter",
    fields=list(
        sourceHost="character",
        sourcePath="character",
        localZip="character",
        localFiles="character"
    ),
    methods=list(
        initialize=function(source_host, source_path, local_zip=NA_character_,
                            local_files=NA_character_, ...) {
            .self$sourceHost <<- source_host
            .self$sourcePath <<- source_path
            if (is.na(local_zip)) {
                .self$localZip <<- sourceFile()
            }
            .self$localFiles <<- local_files
            callSuper(...)
        },
        # Return only the file name from the requested URL
        sourceFile=function() {
            return(base::basename(URLdecode(.self$sourcePath)))
        },
        # Construct a fully qualified URL for the location of the remote data.
        sourceUrl=function() {
            paste(sourceHost, sourcePath, sep="/")
        },
        # Shoe the contents of ParticulateMatter object.
        show=function() {
            cat("Class:: ParticulateMatter\n")
            cat("source_host::", sourceHost, "\n")
            cat("source_path::",sourcePath,"\n")
            cat("local_zip::", localZip, "\n")
            cat("local_files::", localFiles, "\n")
        },
        # Only download the remote file if the localZip file does not exist.
        getRemoteData=function() {
            if (!file.exists(localZip)) {
                download.file(sourceUrl(), localZip, method="curl")
            }
            xfiles <- NULL
            if (!is.na(localFiles)) {
                xfiles <- locaFiles
            }
            .self$localFiles <- unzip(localZip, list=TRUE)[,"Name"]
            unzip(localZip, xfiles, junkpaths=TRUE)
        },
        # Read in the NEI RDS file if-and-only-if the NEI class does not
        # exists in the parent environment and it is not NULL.
        getNEI=function() {
            readit <- TRUE
            if (exists("NEI")) {
                if (!is.null(NEI)) {
                    readit <- FALSE
                }
            }
            if(readit) {
                readRDS("summarySCC_PM25.rds")
            } else {
                return(NEI)
            }
        },
        # Read in the SCE RDS file if-and-only-if the NEI class does not
        # exists in the parent environment and it is not NULL.
        getSCC=function() {
            readit <- TRUE
            if (exists("SCC")) {
                if (!is.null(SCC)) {
                    readit <- FALSE
                }
            if (readit)
                readRDS("Source_Classification_Code.rds")
            } else {
                return(SCC)
            }
        },
        # Write the results of the plotting function to a PNG file
        # parameters:
        # filename = the resulting PNG file
        # plotfn = the function performing the plotting
        plot2png = function(filename, plotfn) {
            png(filename, width=480, height=480)
            plotfn()
            dev.off()
        }
    )
)

#ParticulateMatter$accessors(c("localFiles"))
