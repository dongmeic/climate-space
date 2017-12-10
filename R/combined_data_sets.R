# read datasets
path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"

df.btl.t <- read.csv(paste0(path,"climatic_changes_data_tmp_2.csv"))
df.btl.t <- df.btl.t[,-1]
df.btl.p <- read.csv(paste0(path,"climatic_changes_data_pre_2.csv"))
df.btl.p <- df.btl.p[,-1]
df.btl <- cbind(df.btl.t[,1:8],df.btl.p[,1:7],df.btl.t[,9:10])

df.ldp.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_ldp.csv"))
df.ldp.t <- df.ldp.t[,-1]
df.ldp.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_ldp.csv"))
df.ldp.p <- df.ldp.p[,-1]
df.ldp <- cbind(df.ldp.t[,1:8],df.ldp.p[,1:7],df.ldp.t[,9:10])
df.ldp <- df.ldp[df.ldp$ldp_yr >= 2000 & df.ldp$ldp_yr <= 2014,]
colnames(df.ldp) <- colnames(df.btl)

df.pdr.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_pdr.csv"))
df.pdr.t <- df.pdr.t[,-1]
df.pdr.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_pdr.csv"))
df.pdr.p <- df.pdr.p[,-1]
df.pdr <- cbind(df.pdr.t[,1:8],df.pdr.p[,1:7],df.pdr.t[,9:10])
df.pdr <- df.pdr[df.pdr$pdr_yr >= 2000 & df.pdr$pdr_yr <= 2014,]
colnames(df.pdr) <- colnames(df.btl)

df.wbk.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_wbk.csv"))
df.wbk.t <- df.wbk.t[,-1]
df.wbk.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_wbk.csv"))
df.wbk.p <- df.wbk.p[,-1]
df.wbk <- cbind(df.wbk.t[,1:8],df.wbk.p[,1:7],df.wbk.t[,9:10])
df.wbk <- df.wbk[df.wbk$wbk_yr >= 2000 & df.wbk$wbk_yr <= 2014,]
colnames(df.wbk) <- colnames(df.btl)

df.jck.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_jck.csv"))
df.jck.t <- df.jck.t[,-1]
df.jck.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_jck.csv"))
df.jck.p <- df.jck.p[,-1]
df.jck <- cbind(df.jck.t[,1:8],df.jck.p[,1:7],df.jck.t[,9:10])
df.jck <- df.jck[df.jck$jck_yr >= 2000 & df.jck$jck_yr <= 2014,]
colnames(df.jck) <- colnames(df.btl)

df <- rbind(df.btl,df.ldp,df.jck,df.pdr,df.wbk)
write.csv(df, paste0(path, "climatic_changes_data_combined.csv"))