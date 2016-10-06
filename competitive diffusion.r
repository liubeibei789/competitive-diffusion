# privacy aware competitive diffusion
# by Beibei 
# edited 07/07/2016


# input parameters:
strength1 = 8
strength2 = 6
N = 200

# library:
library(igraph)

# create a graph -- case 1:BA graph
		
		#g = barabasi.game(N,directed = FALSE)
		#plot(g)

# create a graph -- case 2: real data from SNAP
		 
		 #network_oregon1 = read.table("oregon1.txt", header = F)
		  #g = graph.data.frame(network_oregon1, directed = F)
		  #N = vcount(g) 
		
# create a graph -- case 3: complete graph 
		
		#g = graph.full(N, directed = FALSE)
		#plot(g)
		
# create a graph -- case 4: barbell network

        network_barbell = read.csv("bar3.csv", header = F)
		g = graph.data.frame(network_barbell, directed = F)
		N = vcount(g) 
         
		
		#is_connected = vertex.connectivity(g, source=NULL, target=NULL, checks=TRUE)
		#while (is_connected == 0)
		#{
		#	g = graph.full(N, directed = FALSE)
		#	is_connected = vertex.connectivity(g, source=NULL, target=NULL, checks=TRUE)
		#}

			
		largest_eig = graph.eigen(g, which=list(pos="LM", howmany=1))$values
		
		while (largest_eig < 0 )
		{
			largest_eig = graph.eigen(g, which=list(pos="LM", howmany=1))$values
		}
		
# parameters:
			
		# 参数beta决定了整个反应的速率。即beta越大（如0.1）则非常快增长和消减；beta越小，变化越缓慢
   
		beta1 = 0.001
		delta1 = beta1 * largest_eig / strength1
		beta2 = 0.001
		delta2 = beta2 * largest_eig / strength2
		
		# ============ convert to probability ================
						
		 # beta1 = 0.000095
		 # delta1 = 0.01
		 # beta2 = 0.00015
		 # delta2 = 0.01
		
		
		beta1 = 1 - exp(-beta1)
		delta1 = 1 - exp(-delta1) 
		beta2 = 1 - exp(-beta2) 
		delta2 = 1 - exp(-delta2) 
		
		          # beta是以所有感染的节点为基数的，delta是以所有健康的节点为基数的 ？？？？
		
		# 这个是原论文的follow-up中采用的参数
		 # beta1 = 0.007
		 # delta1 = 0.04
		 # beta2 = 0.00045
		 # delta2 = 0.05
		
		

		 coins = c(1, 0)   
		 probabilities_beta1 = c(beta1, 1-beta1 )    
		 probabilities_delta1 = c(delta1, 1-delta1 )          
		 probabilities_beta2 = c(beta2, 1-beta2 )  
		 probabilities_delta2 = c(delta2, 1-delta2 )  
			 
	############ 考虑privacy的情况 ###########################

		privacy_prsv = matrix( 0, nrow = 3, ncol = 3 )

		for (i in 1:3)
			privacy_prsv[i,i] = 1

		privacy_prsv = matrix(c(1,0,0,0,0.5,0.5,0,0.5,0.5),ncol = 3, nrow = 3)
		#privacy_prsv = matrix(c(1,0,0,0,0.7,0.7,0,0.3,0.3),ncol = 3, nrow = 3)
		#privacy_prsv = matrix(c(1,0,0,0,0.23,0.23,0,0.77,0.77),ncol = 3, nrow = 3)
		#privacy_prsv = matrix(c(1,0,0,0,0.23,0.41,0,0.77,0.59),ncol = 3, nrow = 3)
		 
		 probabilities_privacy_22 = c( privacy_prsv[2,2], 1-privacy_prsv[2,2] )     # 0.23
		 probabilities_privacy_23 = c( privacy_prsv[2,3], 1-privacy_prsv[2,3] )     # 0.77
		 probabilities_privacy_32 = c( privacy_prsv[3,2], 1-privacy_prsv[3,2] )     # 0.41
		 probabilities_privacy_33 = c( privacy_prsv[3,3], 1-privacy_prsv[3,3] )     # 0.59

		
# initializaton:

		# sets:
		infected1 = NULL
		infected2 = NULL
		susceptible = NULL
		
		infected1_record = NULL   # for gif-plot purpose 
		infected2_record = NULL

		infected1_report = NULL   # for privacy preserving
		infected2_report = NULL
		
		recover1_record = NULL
		recover2_record = NULL

		new_infected1_record = NULL
		new_infected2_record = NULL
		
		# initial infected nodes:
		sampleNum1 = 30  
		sampleNum2 = 45
		set.seed(2016); infected1 = sample(V(g),sampleNum1) 
		infected1 = setdiff(infected1, NULL)
		tmp = setdiff(V(g), infected1)		
		set.seed(2017); infected2 = sample(tmp,sampleNum2) 
		susceptible = setdiff(V(g), infected1)
		susceptible = setdiff(susceptible, infected2)
		
		infected1_report = infected1   # for privacy preserving
		infected2_report = infected2
		
		carry = list(infected1, infected2, susceptible, infected1_report, infected2_report, recover1_record, recover2_record, new_infected1_record, new_infected2_record)
		# carry[[1]] = infected1
		# carry[[2]] = infected2
		# carry[[3]] = susceptible
		# carry[[4]] = infected1_report
		# carry[[5]] = infected2_report
		
		#record = list()
		totalTime = 1
		
		
# functions:

		toss_beta1 = function(freq) 
		{      
		  tossing = NULL    
		  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities_beta1) 
		  tossing = sum(tossing)   
		  return (tossing)
		}

		toss_beta2 = function(freq) 
		{      
		  tossing = NULL   
		  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities_beta2 ) 								
		  tossing = sum(tossing)   
		  return (tossing)
		}
		
		
	# privacy:

		toss_privacy_22 = function(freq) 
		{      
		  tossing = NULL    
		  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities_privacy_22) 
		  tossing = sum(tossing)   
		  return (tossing)
		}

		toss_privacy_23 = function(freq) 
		{      
		  tossing = NULL    
		  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities_privacy_23) 
		  tossing = sum(tossing)   
		  return (tossing)
		}
		
		toss_privacy_32 = function(freq) 
		{      
		  tossing = NULL    
		  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities_privacy_32) 
		  tossing = sum(tossing)   
		  return (tossing)
		}

		toss_privacy_33 = function(freq) 
		{      
		  tossing = NULL    
		  for (i in 1:freq ) tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities_privacy_33) 
		  tossing = sum(tossing)   
		  return (tossing)
		}
		

	
	
# ************************************************************************************************************************
# ********************************************* 更新各个点感染状态向量的方程 ********************************************
# ************************************************************************************************************************

# carry是个二维数组：carry[[1]]: 被virus 1感染的节点下标 向量
#                    carry[[2]]: 被virus 2感染的节点下标 向量
#                    carry[[3]]: 健康节点下标 向量
#                    carry[[4]]: 因为存在privacy concern，对外报告自己感染virus 1的节点下标 向量
#					 carry[[5]]: 因为存在privacy concern，对外报告自己感染virus 2的节点下标 向量
	
		# node status update:
		set_update = function(carry)
		{
				infected1 = carry[[1]]
				infected2 = carry[[2]]
				susceptible = carry[[3]]
				infected1_report = carry[[4]]
				infected2_report = carry[[5]]
						
			
				dummy = list()
				dummy = 1           # 加一个傀儡节点，确保节点数目不会减到零（那样程序会出错），本实验节点数量巨大不在乎多一个
				
			# ===========================================================================================================
			#                             stage 1: number of susceptible nodes decrease
			# ===========================================================================================================	
			
				new_infected1 = NULL    # 因为要依次遍历完所有的健康节点，才算完成一次update。所以在本次update未完成之前，不能修改健康节点的状态，否则会影响其他健康节点的情况
				new_infected2 = NULL    # 这三个变量，就是临时记录 本次update 产生的变化：新增的感染virus1的节点列表，新增的感染virus2的节点列表，susceptible_reduce是前两个集合的并集
				susceptible_reduce = NULL   # 一个给定的健康节点，只能加入new_infected1或者new_infected2中的一个
							
				for (i in 1:length(susceptible))         # 遍历所有的健康节点，下面考虑第i个健康节点
				{
					neighbors = data.frame(table(unlist(neighborhood(g, 1, susceptible[i]))))    # neighbors：第i个健康节点的邻居
															 				
					neighbors_potential1 = subset(neighbors, (neighbors[,1]%in%infected1_report))   # neighbors_potential1：第i个健康节点的邻居中，宣称感染virus1的节点（即具备传染virus1能力的邻居）
					neighbors_potential2 = subset(neighbors, (neighbors[,1]%in%infected2_report)) 
					
					tossing1 = unlist(lapply(neighbors_potential1[,2], toss_beta1))   # 按beta强度，在具备传染virus1能力的邻居中，抽样决定真正感染成功的邻居1,0序列（对应具备感染能力的邻居列表）
					tossing2 = unlist(lapply(neighbors_potential2[,2], toss_beta2)) 
					
					neighbors_succeed1 = sum(tossing1)   # neighbors_succeed1：真正传染第i个节点virus1的邻居总数（相当于被感染了多少回）
					neighbors_succeed2 = sum(tossing2)
							
					if( !((neighbors_succeed1==0) && (neighbors_succeed2 == 0)) )   # 如果两个virus都没有感染到这个节点，则它保持健康。否则if条件成立，进入内部
					{
						if(neighbors_succeed1 > neighbors_succeed2)       # 被1感染的次数比较大，则该健康节点最终被判定为被1感染
						{
							new_infected1 = append(new_infected1,susceptible[i])   # 把第i个健康节点加入临时存放变化的集合
						}	
						else if(neighbors_succeed1 < neighbors_succeed2)  
						{
							new_infected2 = append(new_infected2,susceptible[i])
						}
						else
						{
						    if( rnorm(1)>0 )                               # 如果被两个virus感染的次数一样多，则用随机数决定
							{
								new_infected1 = append(new_infected1,susceptible[i])
							}
							else
							{
								new_infected2 = append(new_infected2,susceptible[i])
							}
						}
						susceptible_reduce = append(susceptible_reduce,susceptible[i])
					}
					
				}
				
				            # 等所有的健康节点考虑完了，就可以更新各个集合了
							
				infected1 = append(infected1, new_infected1)   # 把“原来是健康的，现在是被1感染”的节点加入infected1
				infected2 = append(infected2, new_infected2)
				susceptible = setdiff(susceptible, susceptible_reduce)  # 从健康节点集合中除掉被感染的节点
				
				susceptible = append(susceptible, dummy)   # 担心除掉被感染节点后健康节点没了，就加一个dummy节点1
				
			# ===========================================================================================================
			#                            stage 2: number of susceptible nodes increase
			# ===========================================================================================================
			
				recover1 = NULL
				recover2 = NULL
				for (i in 1: length(infected1))         # 遍历所有被1感染的节点
				{
					recover1[i] = sample( coins, 1, rep=TRUE, prob= probabilities_delta1 )    # 用delta1为概率，从disease1恢复。得到从disease1恢复的1,0序列
				}
				#new_recover_1 = as.numeric(as.character(infected1[recover >= 1])) 	
				new_recover_1 = infected1[recover1 >= 1]      # 把恢复了的节点加入new_recover_1中
				#class(new_recover_1) <- "igraph.vs"     
					
				recover = NULL  
				for (i in 1: length(infected2)) 
				{
					recover2[i] = sample( coins, 1, rep=TRUE, prob= probabilities_delta2 ) 
				}
				#new_recover_2 = as.numeric(as.character(infected2[recover >= 1])) 
				new_recover_2 = infected2[recover2 >= 1]
				#class(new_recover_2) <- "igraph.vs" 	
					
				susceptible = append(susceptible, new_recover_1)     # 把new_recover_1，new_recover_2添加到健康节点的列表中
				susceptible = append(susceptible, new_recover_2)
				
				infected1 = setdiff(infected1, new_recover_1)  # 把infected1,2 中恢复了的节点去掉
				infected2 = setdiff(infected2, new_recover_2)
				
				infected1 = append(infected1, dummy)   # 为防止减空，加了dummy
				infected2 = append(infected2, dummy)
		        
			# ===========================================================================================================	
			#                                          stage 3: privacy preserving
			# ===========================================================================================================
			
				class(infected1) = "igraph.vs"  # 这个命令的作用是把普通的数字序列转换为vertex结构
				class(infected2) = "igraph.vs"
				 
				 infected1 = data.frame(table(unlist(infected1)))
				 infected2 = data.frame(table(unlist(infected2))) 
				 								
				temp22 = unlist(lapply(infected1[,2], toss_privacy_22))   # 本来是在infected1集合中，结果report 被1感染（如实汇报） 的0,1列表
				temp32 = unlist(lapply(infected2[,2], toss_privacy_32))
						
				report_22 = as.numeric(as.character(infected1[,1][temp22 >= 1]))   # 把0,1列表转换为下标列表
				report_32 = as.numeric(as.character(infected2[,1][temp32 >= 1])) 
				
				infected1_report = append(report_22, report_32)   # 合并：得所有的report 1的下标序列。report_22和report_32不会有重合元素，因为一个节点不能同时被1和2感染


				# temp23 = unlist(lapply(infected1[,2], toss_privacy_23))
				# temp33 = unlist(lapply(infected2[,2], toss_privacy_33))
				
				report_23 = as.numeric(as.character(infected1[,1][temp22 < 1]))   # 本来是1，report2 的下标序列
				report_33 = as.numeric(as.character(infected2[,1][temp32 < 1])) 
				
				infected2_report = append(report_23, report_33)   # 合并：得所有的report 2的下标序列
				
				infected1 = as.numeric(infected1[[1]])    # 转换为数值格式
				infected2 = as.numeric(infected2[[1]]) 
				
				carry[[1]] = infected1
				carry[[2]] = infected2
				carry[[3]] = susceptible
				carry[[4]] = infected1_report
				carry[[5]] = infected2_report
				carry[[6]] = append(new_recover_1,dummy)
				carry[[7]] = append(new_recover_2,dummy)
				carry[[8]] = append(new_infected1,dummy)
				carry[[9]] = append(new_infected2,dummy)
				 
				# please note: carry[[6]] to carry[[9]] 是中间结果，测试时用来print每次新感染的节点下标的（最终程序不使用）
				
				return(carry)
				
		}	
		

##############################################################################################################	
		                      # main function : 整个大循环（多次运行取平均）
 ##############################################################################################################

	num_cum_1 = list()
	num_cum_2 = list()
	num_cum_3 = list()

	#totalTime = 1
for (arvg in 1:1)			# 一共进行5次 取平均值画图  
{	                        # 现在问题在于：第二次试验前注意要把上次的数据清零？？？？
		# sets:
		infected1 = NULL
		infected2 = NULL
		susceptible = NULL
		
		infected1_record = NULL   # for gif-plot purpose 
		infected2_record = NULL

		infected1_report = NULL   # for privacy preserving
		infected2_report = NULL
		
		recover1_record = NULL
		recover2_record = NULL

		new_infected1_record = NULL
		new_infected2_record = NULL
		
		# initial infected nodes:
		sampleNum1 = 30
		sampleNum2 = 45
		set.seed(2016); infected1 = sample(V(g),sampleNum1) 
		infected1 = setdiff(infected1, NULL)
		tmp = setdiff(V(g), infected1)		
		set.seed(2017); infected2 = sample(tmp,sampleNum2) 
		susceptible = setdiff(V(g), infected1)
		susceptible = setdiff(susceptible, infected2)
		
		infected1_report = infected1   # for privacy preserving
		infected2_report = infected2
		
		carry = list(infected1, infected2, susceptible, infected1_report, infected2_report, recover1_record, recover2_record, new_infected1_record, new_infected2_record) 
		
		for (totalTime in 1:100)
		{
			 infected1_record[[totalTime]] = carry[[1]]
			 infected2_record[[totalTime]] = carry[[2]]
			 			 
			 cat(length(carry[[1]]), "->")
			 #cat(length(carry[[2]]), "-->")
			 	 
			 carry_new = set_update(carry)
		     carry = carry_new
		}
		# 每次执行完上面这个for loop，就会得到一个有2000个list的infected1_record（infected2_record），其中每条list都是一个vector(下标序列)
		
		m = totalTime-1
		
        # 每个num_cum_1[[arvg]]都是一个长度为2000的vector，每个元素是原来的vector(下标序列)求和的结果
		# arvg = 1:5
		# 于是，num_cum_1是个有5条list的二位数组（类似），每个list是长度为2000的vector
		
		num_cum_1[[arvg]] = unlist(lapply(1:m, function(x) length(infected1_record[[x]]) ))
		num_cum_2[[arvg]] = unlist(lapply(1:m, function(x) length(infected2_record[[x]]) ))
		num_cum_3[[arvg]] = unlist(lapply(1:m, function(x) (N-length(infected1_record[[x]])-length(infected1_record[[x]]) )  ))
			
}	
	
		arvg1 = rep(0,totalTime-1)
		arvg2 = rep(0,totalTime-1)
		arvg3 = rep(0,totalTime-1)
		for (ii in 1:1)
			arvg1 = arvg1 + num_cum_1[[ii]]    # 横向求和，求平均 
		arvg1 = arvg1/1
		
		for (jj in 1:1)
			arvg2 = arvg2 + num_cum_2[[jj]]
		arvg2 = arvg2/1
		
		for (mm in 1:1)
			arvg3 = arvg3 + num_cum_3[[mm]]
		arvg3 = arvg3/1
		  
		  
# ***********************  作图 ***************************************************** 
 
# 图1：红色的，是virus 1，较强的那个，图2：蓝色的，是virus 2，较弱的那个
# ylim 可以改图的纵坐标显示范围
		  
		time = 1:m
		plot(arvg1~time, type = "b",   
				ylab = "CDF", xlab = "Time", ylim=c(0, 100), col="red")
				
		par(new=TRUE)
		
		plot(arvg2~time, type = "b",   
			ylab = "CDF", xlab = "Time", ylim=c(0, 100), col="blue")
			
# 图3：健康的节点数目（注释掉了）
								
		#plot(arvg3~time, type = "b",   
			#ylab = "CDF", xlab = "Time")
	

	
