#library needed for cURL calls to smartsheet API
library("httr", lib.loc="~/R/R-3.4.1/library")

#Auth call using the Smartsheet private key. 
r <- GET("https://api.smartsheet.com/2.0/sheets"
         ,add_headers(Authorization = "Bearer xxxxxxxxxxxxxxxxxxxxxxxxx"),accept_json()
     )
#get the response content
proyectos <- content(r, "parsed")[[5]]


# loop trough projects list
for(unproyecto in proyectos){
  
  #prepre the list for echa project iteration
  lista_final <- list() 
  
  #prepare the common varibles related to project info
  id_proyecto <- as.character(unproyecto$id)
  nombre_proyecto <- unproyecto$name
  enlace_proyecto <- unproyecto$permalink
  
  #call to get the smartsheet project
  r2 <- GET(paste("https://api.smartsheet.com/2.0/sheets/",id_proyecto,sep = "")
            ,add_headers(Authorization = "Bearer xxxxxxxxxxxxxxxxxxxxxxxxx"),accept_json()
  )
  #get the columns from response 
  columnas <- content(r2, "parsed")$columns
  
  columnas_final <- c()
  
  #create a vector from column names
  for(col in columnas){
    columnas_final <- c(columnas_final,col$title)
  }
  columnas_final <- gsub(" ","_",columnas_final)
  
  #get the rows from response 
  filas <- content(r2, "parsed")$rows
  
  # loop trough rows
  for(x in filas){
    #prepare the common variables
    row_id <- as.character(x$id)
    row_number <- x$rowNumber
    row_fec_ini <- as.character(x$createdAt)
    row_fec_mod <- as.character(x$modifiedAt)
    row_parent_id <- ifelse(is.null(x$parentId),"",x$parentId)
    row_sibling_id <- ifelse(is.null(x$siblingId),"",x$siblingId)
    
    # common variables temp vector
    tmp <- c(id_proyecto
             ,nombre_proyecto
             ,enlace_proyecto
             ,as.character(row_number)
             ,row_id,row_fec_ini
             ,row_fec_mod
             ,row_parent_id,row_sibling_id
           )
    
    #add the specific cells related to each project
    for(cell in x$cells){
      tmp <- c(tmp,ifelse(is.null(cell$value),"",cell$value))  
    }
    #create the list item that represents a single row
    lista_final[[row_number]] <- tmp 
  }
  #create data frame from the list 
  tabla_final <- do.call("rbind",lista_final)
  #associate the colum names
  colnames(tabla_final) <- c("id_proyecto"
                             ,"nombre_proyecto"
                             ,"enlace_proyecto"
                             ,"orden","id"
                             ,"fecha_inicio"
                             ,"fecha_modificacion"
                             ,"padre"
                             ,"pariente"
                             ,columnas_final
                            )
  #generate the csv product related to each project  
  write.csv(tabla_final,file = paste("res/",nombre_proyecto,".csv",sep=""))
}
