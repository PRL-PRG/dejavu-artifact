library(RMySQL)

# println + concatenation wrapper
println <- function(...) {
    cat(paste(..., "\n", sep = ""))
}

plain <- function(x,...) {
    format(x, ..., scientific = FALSE, trim = TRUE)
}

paste.path <- function(folder, filename) {
    paste(folder, filename, sep = "/")
}

# heatmaps ------------------------------------------------------------------------------------------------------------

createHeatmap <- function(datasetName, datasetPath, y_axis = "commits") {
    data = read.csv(paste(datasetPath, "/heatmap.csv", sep=""), header = F, col.names = c("pid", "stars", "commits", "files", "originalFiles","containsClones"))
    # create the matrix
    len_x = ceiling(log10(max(data$files) + 1) / 0.2) + 1
    len_y = ceiling(log10(max(data[[y_axis]]) + 1) / 0.2) + 1
    # because the hashmaps for the paper are cropped and we want to use the same function 
    len_x = max(25, len_x)
    len_y = max(25, len_y)
    density = matrix(0, len_x, len_y)
    sumFiles = matrix(0, len_x, len_y)
    sumOriginalFiles = matrix(0, len_x, len_y)
    sumContainsClones = matrix(0, len_x, len_y) 
    total = 0
    for (i in 1:length(data$files)) {
        x = ceiling(log10(data$files[[i]] + 1) / 0.2) + 1
        y = ceiling(log10(data[[y_axis]][[i]] + 1) / 0.2) + 1
        density[x,y] = density[x,y] + 1
        sumFiles[x,y] = sumFiles[x,y] + data$files[[i]]
        sumOriginalFiles[x,y] = sumOriginalFiles[x,y] + data$originalFiles[[i]]
        sumContainsClones[x,y] = sumContainsClones[x,y] + data$containsClones[[i]]
        total = total + 1
    }
    # convert the matrix to data.frame
    files = double()
    stars = double()
    d = double()
    sf = double()
    sof = double()
    scc = double()
    originality = double()
    avgClones = double()
    #for (i in 1:len_x) {
    #    for (j in 1:len_y) {
    for (i in 3:25) {
        for (j in 3:25) {
            files = c(files, (i - 1) * 0.2)
            stars = c(stars, (j - 1) * 0.2)
            d = c(d, density[i,j])
            sf = c(sf, sumFiles[i,j])
            sof = c(sof, sumOriginalFiles[i,j])
            scc = c(scc, sumContainsClones[i,j])
            originality = c(originality, sumOriginalFiles[i, j] / sumFiles[i,j])
            avgClones = c(avgClones, sumContainsClones[i, j] / density[i,j])
        } 
    }
    heat_data = data.frame(files = files, yaxis = stars, density = d, Clones = originality, avgClones = avgClones, sumFiles = sf, sumOriginalFiles = sof, sumContainsClones = scc)
    # do the graph
    g = ggplot(heat_data, aes(files, yaxis))
    g = g + geom_raster(aes(fill = Clones))
    #g = g + scale_fill_gradient(low = "red", high = "green", limits = c(1, 0), breaks = c(0.25, 0.5, 0.75))
    g = g + scale_fill_gradient(low = "white", high = "red", limits = c(1, 0), breaks = c(0.25, 0.5, 0.75))
    g = g + scale_x_continuous("Files per Project", labels = function(x) plain(10**x))
    g = g + scale_y_continuous("Commits", labels = function(x) plain(10**x))
    g = g + geom_text(aes(label = round(100 - Clones * 100, 0)), size = 3)
    g = g + theme(panel.background = element_rect(color = "black", fill="white"), panel.spacing = unit(c(0,0,0,0), "points"))
    g = g + coord_cartesian(c(0.45, 4.75), c(0.45, 4.75))
    #g = g + scale_fill_continuous(limits=c(1, 0), breaks=seq(1,0,by=-0.25))
    g = g + theme(axis.title.x=element_blank(), axis.title.y = element_blank())
    ggsave(paste("graphs/", datasetName, "/heatmap.pdf", sep = ""), width = 68 * 2.5, height = 68 * 2.5, units = "mm")
    g
}

# graphs --------------------------------------------------------------------------------------------------------------

logHistogram <- function(dbname, query, title, xtitle, ytitle, filename = NULL) {
    linetypes = c("Median" = "solid", "Mean" = "dashed")
    
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbname, host = DB_HOST)
    query = sql.query(query)[[1]]
    sql.disconnect()
    # because we are log hist, do log
    #query = log10(query + 1)
    data = data.frame(x = query)
    # calculate the breaks so that we fill the range
    breaks = 1
    i = 1
    m = max(query)
    repeat {
        i = i * 10
        breaks = c(breaks, i + 1)
        if (log10(i + 1) > m)
            break
    }
    breaks = log10(breaks)
    # draw the graph
    g = ggplot(data)
    g = g + geom_histogram(binwidth = 0.2, boundary = 0, aes(x = log10(x + 1), y=..count../sum(..count..)))
    # add mean & median vertical lines
    x_mean = log10(mean(query) + 1)
    x_median = log10(median(query) + 1)
    g <- g + geom_vline(aes(xintercept = x_mean, linetype = "Mean"), alpha = 1)
    g <- g + geom_vline(aes(xintercept = x_median, linetype = "Median"), alpha = 1)
    g = g + scale_x_continuous(xtitle, labels = function(x) plain(10**x - 1), breaks = breaks) + theme(axis.text.y = element_text(angle=90, hjust = 0.5)) + scale_y_continuous(ytitle, labels=function(x) x * 100)
    g = g + scale_linetype_manual(name="Statistics", values = linetypes)
    
    g = g + ggtitle(title)
    g = g + theme(plot.title = element_text(hjust = 0.5))
    if (!is.null(filename))
        ggsave(paste("graphs/", dbname, "/filename", ".pdf", sep=""), width = 68 * 2.5, height = 55 * 2.5, units = "mm")
    g
}

normalHistogram <- function(dbname, query, title, xtitle, ytitle, filename = NULL) {
    linetypes = c("Median" = "solid", "Mean" = "dashed")
    
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbname, host = DB_HOST)
    query = sql.query(query)[[1]]
    sql.disconnect()
    data = data.frame(x = query)
    # draw the graph
    g = ggplot(data)
    g = g + geom_histogram(aes(x = x, y=..count../sum(..count..)))
    # add mean & median vertical lines
    x_mean = mean(query)
    x_median = median(query)
    g <- g + geom_vline(aes(xintercept = x_mean, linetype = "Mean"), alpha = 1)
    g <- g + geom_vline(aes(xintercept = x_median, linetype = "Median"), alpha = 1)
    g = g + scale_x_continuous(paste(xtitle)) + theme(axis.text.y = element_text(angle=90, hjust = 0.5)) + scale_y_continuous(ytitle, labels=function(x) x * 100)
    g = g + scale_linetype_manual(name="Statistics", values = linetypes)
    
    g = g + ggtitle(title)
    g = g + theme(plot.title = element_text(hjust = 0.5))
    if (!is.null(filename))
        ggsave(paste("graphs/", dbname, "/filename", ".pdf", sep=""), width = 68 * 2.5, height = 55 * 2.5, units = "mm")
    g
}

normalHistogramLogY <- function(dbname, query, title, xtitle, ytitle, filename = NULL) {
    linetypes = c("Median" = "solid", "Mean" = "dashed")
    
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbname, host = DB_HOST)
    query = sql.query(query)[[1]]
    sql.disconnect()
    data = data.frame(x = query)
    # draw the graph
    g = ggplot(data)
    g = g + geom_histogram(aes(x = x), bins = 50)
    # add mean & median vertical lines
    x_mean = mean(query, na.rm = T)
    x_median = median(query, na.rm = T)
    g <- g + geom_vline(aes(xintercept = x_mean, linetype = "Mean"), alpha = 1)
    g <- g + geom_vline(aes(xintercept = x_median, linetype = "Median"), alpha = 1)
    g = g + scale_x_continuous(paste(xtitle)) + theme(axis.text.y = element_text(angle=90, hjust = 0.5))
    g = g + scale_linetype_manual(name="Statistics", values = linetypes)
    g = g + ggtitle(title)
    g = g + theme(plot.title = element_text(hjust = 0.5))
    g = g + scale_y_log10(ytitle)
    if (!is.null(filename))
        ggsave(paste("graphs/", dbname, "/filename", ".pdf", sep=""), width = 68 * 2.5, height = 55 * 2.5, units = "mm")
    g
}

logHistogramFromDF <- function(data, column, title, xtitle, ytitle, filename = NULL, summary = T) {
    linetypes = c("Median" = "solid", "Mean" = "dashed")
    
    data = data.frame(x = data[[column]])
    # calculate the breaks so that we fill the range
    breaks = 1
    i = 1
    m = max(data$x)
    repeat {
        i = i * 10
        breaks = c(breaks, i + 1)
        if (log10(i + 1) > m)
            break
    }
    breaks = log10(breaks)
    # draw the graph
    g = ggplot(data)
    if (summary) {
        g = g + geom_histogram(binwidth = 0.2, boundary = 0, aes(x = log10(x + 1), y=..count../sum(..count..)))
        g = g + scale_x_continuous(xtitle, labels = function(x) plain(10**x - 1), breaks = breaks) + theme(axis.text.y = element_text(angle=90, hjust = 0.5)) + scale_y_continuous(ytitle, labels=function(x) x * 100)
    } else  {
        g = g + geom_histogram(binwidth = 0.2, boundary = 0, aes(x = log10(x + 1)))
        g = g + scale_x_continuous(xtitle, labels = function(x) plain(10**x - 1), breaks = breaks) + theme(axis.text.y = element_text(angle=90, hjust = 0.5)) + scale_y_continuous(ytitle, labels=function(x) plain(x))
    }
    # add mean & median vertical lines
    x_mean = log10(mean(data$x) + 1)
    x_median = log10(median(data$x) + 1)
    g <- g + geom_vline(aes(xintercept = x_mean, linetype = "Mean"), alpha = 1)
    g <- g + geom_vline(aes(xintercept = x_median, linetype = "Median"), alpha = 1)
    g = g + scale_linetype_manual(name="Statistics", values = linetypes)
    
    g = g + ggtitle(title)
    g = g + theme(plot.title = element_text(hjust = 0.5))
    if (!is.null(filename))
        ggsave(paste("graphs/", title, "/filename", ".pdf", sep=""), width = 68 * 2.5, height = 55 * 2.5, units = "mm")
    g
}

logHistogramDouble = function(db1, query, db1Title, db2Title, title, xtitle, ytitle, filename = NULL, query2 = query) {
    db2 = paste(db1, "_nonpm", sep="")
    colors = c("red", "blue")
    names(colors) = c(db1Title, db2Title)
    linetypes = c("Median" = "solid", "Mean" = "dashed")
    
    # get the input data
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = db1, host = DB_HOST)
    sql.query("USE ", db1)
    first = sql.query(query)[[1]];
    sql.query("USE ", db2)
    second = sql.query(query2)[[1]];
    sql.disconnect()
    
    # make lengths the same
    maxl = max(length(first), length(second))
    length(first) = maxl
    length(second) = maxl
    # create the dataframe
    data = data.frame(first = first, second = second)
    # calculate the breaks so that we fill the range
    breaks = 1
    i = 1
    m = max(max(first, na.rm = T), max(second, na.rm = T))
    repeat {
        i = i * 10
        breaks = c(breaks, i + 1)
        if (log10(i + 1) > m)
            break
    }
    breaks = log10(breaks)
    # create the graph
    
    g = ggplot(data)
    g = g + geom_histogram(binwidth = 0.2, boundary = 0, aes(x = log10(first + 1), y=..count../sum(..count..), fill = db1Title), alpha = 0.5)
    g = g + geom_histogram(binwidth = 0.2, boundary = 0, aes(x = log10(second + 1), y=..count../sum(..count..), fill = db2Title), alpha = 0.5, show.legend = T)
    
    first_mean = log10(mean(first, na.rm = T) + 1)
    first_median = log10(median(first, na.rm = T) + 1)
    g <- g + geom_vline(aes(xintercept = first_mean, linetype = "Mean", color = db1Title), alpha = 0.7)
    g <- g + geom_vline(aes(xintercept = first_median, linetype = "Median", color = db1Title), alpha = 0.7)
    
    second_mean = log10(mean(second, na.rm = T) + 1)
    second_median = log10(median(second, na.rm = T) + 1)
    g <- g + geom_vline(aes(xintercept = second_mean, linetype = "Mean", color = db2Title), alpha = 0.7)
    g <- g + geom_vline(aes(xintercept = second_median, linetype = "Median", color = db2Title), alpha = 0.7)
    
    g = g + scale_x_continuous(xtitle, labels = function(x) plain(10**x - 1), breaks = breaks) + theme(axis.text.y = element_text(angle=90, hjust = 0.5)) + scale_y_continuous(ytitle, labels=function(x) x * 100)
    g = g + scale_fill_manual(name=" ",values = colors)
    g = g + scale_linetype_manual(name="Statistics", values = linetypes)
    g = g + scale_color_manual(values=colors, guide = "none")
    
    g = g + ggtitle(title)
    g = g + theme(plot.title = element_text(hjust = 0.5))
    if (!is.null(filename))
        ggsave(paste("graphs/", db1, "/filename", ".pdf", sep=""), width = 68 * 2.5, height = 55 * 2.5, units = "mm")
    g
}



# database import & data processing -----------------------------------------------------------------------------------

# Imports the dataset (files, stats and projects tables) from csv files produced by the downloader & tokenizer
importDataset <- function(dbName, inputFolder) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbName, host = DB_HOST)
    importCommonData(inputFolder)
    createCommonIndices()
    calculateProjectSizes()
    sql.disconnect()
}

# creates projects, files and stats tables in given database and populates them from the selected folder, which must contain the appropriate output files from the downloader (projects.txt, files.txt and stats.txt)
importCommonData <- function(inputFolder) {
    println("  removing existing tables...")
    println("    ", sql.dropTable("projects"))
    println("    ", sql.dropTable("files"))
    println("    ", sql.dropTable("stats"))
    
    println("  creating tables...")
    println("    ", sql.createTable("projects", "
                                    projectId INT UNSIGNED NOT NULL,
                                    projectPath VARCHAR(4000) NOT NULL,
                                    projectUrl VARCHAR(4000) NOT NULL,
                                    PRIMARY KEY (projectId)"))
    println("    ", sql.createTable("files", "
                                    fileId BIGINT UNSIGNED NOT NULL,
                                    projectId INT UNSIGNED NOT NULL,
                                    relativeUrl VARCHAR(4000) NOT NULL,
                                    fileHash BIGINT NOT NULL,
                                    PRIMARY KEY (fileId)"))
    println("    ", sql.createTable("stats","
                                    fileHash BIGINT NOT NULL,
                                    fileBytes INT NOT NULL,
                                    fileLines INT NOT NULL,
                                    fileLOC INT NOT NULL,
                                    fileSLOC INT NOT NULL,
                                    totalTokens INT NOT NULL,
                                    uniqueTokens INT NOT NULL,
                                    tokenHash BIGINT NOT NULL,
                                    PRIMARY KEY (fileHash)"))
    
    println("  loading tables...")
    println("    ", sql.loadTable("projects", paste(inputFolder, "projects.csv", sep = "/")))
    println("    ", sql.loadTable("files", paste(inputFolder, "files.csv.h2i", sep = "/")))
    println("    ", sql.loadTable("stats", paste(inputFolder, "stats.csv.h2i", sep = "/")))
    
}

# creates indices on projects, files and stats tables. Assumes the database containing the tables has already been selected
createCommonIndices <- function() {
    println("  creating indices...")
    println("    ", sql.createIndex("projects", "projectId"))
    println("    ", sql.createIndex("files", "fileId"))
    println("    ", sql.createIndex("files", "projectId", unique = F))
    println("    ", sql.createIndex("stats", "fileHash"))
    println("    ", sql.createIndex("stats", "tokenHash", unique = F))
}

# augments the projects table with files column and counts for each project number of files it contains 
calculateProjectSizes <- function() {
    println("calculating project sizes...")
    sql.query("ALTER TABLE projects ADD COLUMN files INT UNSIGNED NOT NULL DEFAULT 0")
    println("  added column files to projects table")
    sql.query("CREATE TABLE projects_files SELECT COUNT(*) AS files, projectId AS pid FROM files GROUP BY files.projectId")
    println("  file counts calculated")
    sql.query("UPDATE projects JOIN projects_files ON projects.projectId = projects_files.pid SET projects.files = projects_files.files")
    println("  projects table updated")
    sql.query("DROP TABLE projects_files;")
    println("  deleted temporary tables")
}

# imports the results of sourcererCC
importSourcerer <- function(dbName, inputFolder) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbName, host = DB_HOST)
    println("  removing existing tables...")
    println("    ", sql.dropTable("CCPairs"))
    println("  creating tables...")
    println("    ", sql.createTable("CCPairs", "
                                    projectId1 INT UNSIGNED NOT NULL,
                                    fileId1 INT UNSIGNED NOT NULL,
                                    projectId2 INT UNSIGNED NOT NULL,
                                    fileId2 INT UNSIGNED NOT NULL,
                                    PRIMARY KEY (fileId1, fileId2)"))
    println("  loading tables...")
    println("    ", sql.loadTable("CCPairs", paste(inputFolder, "sourcerer.csv", sep = "/")))
    println("  creating indices...")
    println("    ", sql.createIndex("CCPairs", "projectId1", unique = F))
    println("    ", sql.createIndex("CCPairs", "fileId1", unique = F))
    println("    ", sql.createIndex("CCPairs", "projectId2", unique = F))
    println("    ", sql.createIndex("CCPairs", "fileId2", unique = F))
    sql.disconnect()
}

# Imports Javascript extra data. Alters projects with time of creation and commit at which the project has been tokenized. Alters files with the time of creation, which can be used for originals detection. Loads the data from files_extra and projects_extra files produced by the JS tokenizer. The data is added as extra columns to the files and projects table, which saves space in the database (as opposed to having an extra table) while not breaking any compatibility, the extra columns simply do not have to be used. 
importJSExtras <- function(dbName, inputFolder) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbName, host = DB_HOST)
    println("importing JS specific data...")
    println("  creating tables...")
    println("    ", sql.createTable("projects_extra", "
        projectId INT UNSIGNED NOT NULL,
        createdAt INT UNSIGNED NOT NULL,
        commit CHAR(40) NOT NULL,
        PRIMARY KEY (projectId)"))
    println("    ", sql.createTable("files_extra", "
        fileId BIGINT NOT NULL,
        createdAt INT UNSIGNED NOT NULL,
        PRIMARY KEY (fileId)"))
    
    println("  loading tables...")
    println("    ", sql.loadTable("projects_extra", paste(inputFolder, "projects_extra.csv", sep = "/")))
    println("    ", sql.loadTable("files_extra", paste(inputFolder, "files_extra.csv", sep = "/")))
    
    println("  merging information...")
    sql.query("ALTER TABLE projects ADD COLUMN createdAt INT UNSIGNED NOT NULL")
    println("    createdAt added to projects")
    sql.query("ALTER TABLE projects ADD COLUMN commit CHAR(40) NOT NULL")
    println("    commit added to projects")
    sql.query("UPDATE projects JOIN projects_extra ON projects.projectId = projects_extra.projectId 
    SET projects.createdAt = projects_extra.createdAt, projects.commit = projects_extra.commit")
    println("    projects table updated")
    sql.query("ALTER TABLE files ADD COLUMN createdAt INT UNSIGNED NOT NULL")
    println("    createdAt added to files")
    sql.query("UPDATE files JOIN files_extra ON files.fileId = files_extra.fileId SET files.createdAt = files_extra.createdAt")
    println("    files table updated")
    
    println("  deleting temporary tables")
    println("    ", sql.dropTable("projects_extra"))
    println("    ", sql.dropTable("files_extra"))
    sql.disconnect()
}

# Imports the infromation about npm files (and extra information about the files in the dataset)
importAndCreateJS_NPM <- function(dbName, dbOrigin, inputFolder) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbOrigin, host = DB_HOST)
    importNPMInfo(inputFolder)
    sql.switchDb(dbName)
    createNonNPMDataset(dbOrigin)
    sql.disconnect()
}

# Augments the files table with the information whether the file belongs to an NPM package or not.
importNPMInfo <- function(inputFolder) {
    println("importing NPM & file origin information")
    println("  creating files_nm table...")
    sql.dropTable("files_nm")
    sql.createTable("files_nm", "
        fileId BIGINT UNSIGNED NOT NULL,
        pathDepth SMALLINT UNSIGNED NOT NULL,
        npmDepth SMALLINT UNSIGNED NOT NULL,
        test TINYINT NOT NULL,
        locale TINYINT NOT NULL,
        moduleName VARCHAR(255) NOT NULL,
        blameModule VARCHAR(255) NOT NULL,
        fileName VARCHAR(1000) NOT NULL,
        fileExt VARCHAR(255) NOT NULL,
        inModuleName VARCHAR(4000) NOT NULL,
        PRIMARY KEY (fileId)")
    println("  loading table...")
    println("    ", sql.loadTable("files_nm", paste(inputFolder, "files_nm.csv", sep = "/")))
    
    # now alter the files table, add package and test categories
    println("  altering files table")
    sql.query("ALTER TABLE files ADD COLUMN npm TINYINT NOT NULL")
    println("    npm column")
    sql.query("ALTER TABLE files ADD COLUMN test TINYINT NOT NULL")
    
    # and merge the information from the files_nm table
    println("  updating the files table...")
    sql.query("UPDATE files JOIN files_nm ON files.fileId = files_nm.fileId SET files.npm = IF(files_nm.npmDepth > 0, 1, 0), files.test = files_nm.test")
    
    # we keep everything in the files_nm table as well, it might be useful in the future?    
}

# Takes the origin database (which should have npm info already present) and creates projects files & stats tables in current database containing only those not in npm modules 
createNonNPMDataset <- function(origin) {
    println("copying only NPM data...")
    sql.query("CREATE TABLE projects AS SELECT projectId, projectUrl, createdAt, commit FROM ", origin, ".projects")
    println("  projects")
    sql.query("CREATE TABLE files AS SELECT fileId, projectId, relativeUrl, fileHash, createdAt, test FROM ", origin, ".files WHERE npm = 0")
    println("  files")
    sql.query("CREATE TABLE stats AS SELECT * FROM ", origin, ".stats WHERE fileHash IN (SELECT DISTINCT fileHash FROM files)")
    println("  stats")
    createCommonIndices()
    calculateProjectSizes()
}

# exports the data about files and projects required to calculate project level cloning
exportCloneFinderData <- function(dbName, outputFolder, threshold = 0) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbName, host = DB_HOST)
    println("exporting clone finder input data")
    println("  creating clone finder's input...")
    sql.query("SELECT projectId, totalTokens, tokenHash FROM files JOIN stats ON files.fileHash = stats.fileHash WHERE totalTokens > ",threshold," INTO OUTFILE \"", outputFolder, "/clone_finder.csv\" FIELDS TERMINATED BY ','");
    sql.disconnect()
}

exportHeatmapData <- function(dbName, outputFolder) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbName, host = DB_HOST)
    println("exporting projects_heat.csv")
    sql.query("SELECT projectId, stars, commits FROM projects INTO OUTFILE \"", outputFolder, "/projects_heat.csv\" FIELDS TERMINATED BY ','");
    sql.disconnect();
}

# when project level cloning is calculated, loads it in the database
importCloneFinderData <- function(dbName, inputFolder, numThreads) {
    sql.connect(username = DB_USER, password = DB_PASSWORD, dbname = dbName, host = DB_HOST)
    println("  creating cf table projectClones")
    println("    dropping if exists", sql.dropTable("projectClones"))
    println("    creating", sql.createTable("projectClones","
        cloneId INT UNSIGNED NOT NULL,
        cloneClonedFiles INT UNSIGNED NOT NULL,
        cloneTotalFiles INT UNSIGNED NOT NULL,
        cloneCloningPercent DECIMAL(6,3) NOT NULL,
        hostId INT UNSIGNED NOT NULL,
        hostAffectedFiles INT UNSIGNED NOT NULL,
        hostTotalFiles INT UNSIGNED NOT NULL,
        hostAffectedPercent DECIMAL(6,3) NOT NULL,
        PRIMARY KEY (cloneId, hostId)"))
    
    println("    loading chunks...")
    for (i in 0:(numThreads - 1)) {
        filename = paste(inputFolder, "/project_clones.", i, ".csv", sep = "")
        println("      ", sql.loadTable("projectClones", filename))
    }
    
    println("    creating indices...")
    println("      ", sql.createIndex("projectClones", "cloneId", unique = F))
    println("      ", sql.createIndex("projectClones", "cloneTotalFiles", unique = F))
    println("      ", sql.createIndex("projectClones", "cloneCloningPercent", unique = F))
    println("      ", sql.createIndex("projectClones", "hostId", unique = F))
    println("      ", sql.createIndex("projectClones", "hostTotalFiles", unique = F))
    println("      ", sql.createIndex("projectClones", "hostAffectedPercent", unique = F))
    sql.disconnect()    
}

downloadMetadata <- function(dataset, outputDir, secrets, stride = 1, strides = 1) {
    library(RCurl)
    library(rjson)
    library(bitops)
    sql.connect(user = DB_USER, password = DB_PASSWORD, dbname = dataset, host = DB_HOST)
    # now get all the projects we want to get metadata for
    projects = sql.query("SELECT projectId, projectUrl FROM projects ORDER BY projectId")
    sql.disconnect()
    numProjects = length(projects$projectId)
    println("total projects: ", numProjects)
    f = file(paste(outputDir, "/projects_metadata-",stride,".csv", sep = ""), "wt")
    si = 1L
    errors = 0
    i = stride
    while (i <= numProjects) {
        secret = secrets[[si]]
        si = si + 1L
        if (si > length(secrets))
            si = 1L
        pid = projects$projectId[[i]]
        tryCatch({
            x = getProjectMetadata(pid, projects$projectUrl[[i]], secret)
            if (x$stars == "NULL") {
                cat(paste(x$id, "-1,-1,-1,-1\n", sep = ","), file = f)
                errors = errors + 1
            } else {
                cat(paste(x$id, x$stars, x$subscribers, x$forks, x$openIssues, sep = ","), file = f)
                cat("\n", file = f)
            }
        }, error = function(e) errors = errors + 1)
        i = i + strides
        if (i %% 1000 == stride)
            println("   ", i, " errors: ", errors)
        
    }
    println("   TOTAL ERRORS: ", errors)
    close(f)
}

getProjectMetadata <- function(pid, url, secret) {
    url = paste("https://api.github.com/repos/", url, sep = "")
    #println(pid)
    result = list(id = pid)
    x = fromJSON(getURL(url, USERAGENT = "prl-prg", FOLLOWLOCATION = T, HTTPHEADER = paste("Authorization: token ", secret, sep = "")))
    result$stars = x["stargazers_count"]
    result$subscribers = x["subscribers_count"]
    result$forks = x["forks_count"]
    result$openIssues = x["open_issues_count"]
    result
}

importMetadata <- function(dataset, inputFolder, strides = 1) {
    sql.connect(user = DB_USER, password = DB_PASSWORD, dbname = dataset, host = DB_HOST)
    sql.dropTable("projects_metadata")
    sql.createTable("projects_metadata", "
        projectId INT NOT NULL,
        stars INT NOT NULL,
        subscribers INT NOT NULL,
        forks INT NOT NULL,
        openIssues INT NOT NULL,
        PRIMARY KEY (projectId)")
    println("    loading chunks...")
    for (i in 1:(strides)) {
        filename = paste(inputFolder, "/projects_metadata-", i, ".csv", sep = "")
        println("      ", sql.loadTable("projects_metadata", filename))
    }
    println("    altering projects table...")
    sql.query("ALTER TABLE projects ADD COLUMN stars INT NOT NULL DEFAULT 0")
    sql.query("ALTER TABLE projects ADD COLUMN subscribers INT NOT NULL DEFAULT 0")
    sql.query("ALTER TABLE projects ADD COLUMN forks INT NOT NULL DEFAULT 0")
    sql.query("ALTER TABLE projects ADD COLUMN openIssues INT NOT NULL DEFAULT 0")
    sql.query("UPDATE projects JOIN projects_metadata ON projects.projectId = projects_metadata.projectId SET
        projects.stars = projects_metadata.stars,
        projects.subscribers = projects_metadata.subscribers,
        projects.forks = projects_metadata.forks,
        projects.openIssues = projects_metadata.openIssues")
    sql.dropTable("projects_metadata");
}

importCommits <- function(dataset, inputFolder) {
    sql.connect(user = DB_USER, password = DB_PASSWORD, dbname = dataset, host = DB_HOST)
    sql.dropTable("projects_commits")
    sql.createTable("projects_commits", "
        projectId INT NOT NULL,
        commits INT NOT NULL,
        PRIMARY KEY (projectId)")
    println("    loading data...")
    filename = paste(inputFolder, "/project_commits.csv", sep = "")
    println("      ", sql.loadTable("projects_commits", filename))
    println("    altering projects table...")
    sql.query("ALTER TABLE projects ADD COLUMN commits INT NOT NULL DEFAULT 0")
    sql.query("UPDATE projects JOIN projects_commits ON projects.projectId = projects_commits.projectId SET
        projects.commits = projects_commits.commits")
    sql.dropTable("projects_commits");
}

# SQL functions -------------------------------------------------------------------------------------------------------

DB_CONNECTION_ = NULL
LAST_SQL_TIME_ = NULL

# connects to the given db server and opens the database name, if the database name does not exist, creates it. Keeps the connection alive 
sql.connect <- function(username, password, dbname, host = "localhost") {
    # disconnect first, if we have existing connection
    sql.disconnect()
    tryCatch({
        # now connect to the database
        DB_CONNECTION_ <<- dbConnect(MySQL(), user = username, password = password, host = host, dbname = dbname)
    }, error = function(e) {
        # if the error is the databse does not exist, create it
        if (length(grep("Failed to connect to database: Error: Unknown database", e$message)) > 0) {
            DB_CONNECTION_ <<- dbConnect(MySQL(), user = username, password = password, host = host)
            sql.query("CREATE DATABASE ", dbname)
            println("Creating database ", dbname)
            sql.disconnect()
            sql.connect(username, password, dbname, host)
        } else {
            stop(e)
        }
    })
}

# disconnects from the database
sql.disconnect <- function() {
    if (! is.null(DB_CONNECTION_)) {
        dbDisconnect(DB_CONNECTION_)
        DB_CONNECTION_ <<- NULL
    }
}

# concatenates the arguments into one string and executes it as query, if updateTime is T, stores the time the query took on server
sql.query <- function(..., updateTime = T) {
    result <- 0
    f <- function() {
        res <- dbSendQuery(DB_CONNECTION_, paste(..., sep = ""))         
        result <<- dbFetch(res, n = -1)
        dbClearResult(res)
    }
    if (updateTime) {
        LAST_SQL_TIME_ <<- system.time({
            f()
        })
    } else {
        f()
    }
    result
}

# returns the time in seconds it took the last query to execute on the server
sql.lastTime <- function() {
    if (is.null(LAST_SQL_TIME_))
        0
    else
        LAST_SQL_TIME_[["elapsed"]]
}

# creates (recreates) index on given table and column
sql.createIndex <- function(table, column, unique = T) {
    index = gsub(",", "", column)
    index = gsub(" ", "", index)
    index = paste("index_", index, sep="")
    x <- sql.query("SHOW INDEX FROM ", table, " WHERE KEY_NAME=\"", index, "\"")$Key_name
    if (length(x) > 0)
        sql.query("DROP INDEX ", index, " ON ", table)
    if (unique)
        sql.query("CREATE UNIQUE INDEX ", index, " ON ", table, "(", column, ")")
    else
        sql.query("CREATE INDEX ", index, " ON ", table, "(", column, ")")
    paste("created index ", column, " on table ", table, " in ", sql.lastTime(), "[s]", sep = "")
}

sql.dropTable <- function(name) {
    sql.query("DROP TABLE IF EXISTS ", name)
    name
}

sql.createTable <- function(name, contents) {
    sql.query("CREATE TABLE ", name, " (", contents, ")")
    name
}


sql.loadTable <- function(name, file) {
    sql.query("LOAD DATA LOCAL INFILE \"", file,"\" INTO TABLE ", name, " FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"'")
    file
}

# switches the database, creating a new one if the given database does not exist
sql.switchDb <- function(dbName) {
    tryCatch({
        sql.query("USE ", dbName)
    }, error = function(e) {
        # if the error is the databse does not exist, create it
        if (length(grep("Unknown database", e$message)) > 0) {
            sql.query("CREATE DATABASE ", dbName)
            sql.query("USE ", dbName)
        } else {
            stop(e)
        }
    })
}

