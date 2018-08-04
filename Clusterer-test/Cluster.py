import numpy as np
import pandas as pd
import os 
from matplotlib import pyplot as plt
from sklearn.cluster.dbscan_ import DBSCAN
from xlrd import open_workbook #http://pypi.python.org/pypi/xlrd
def generate_real_fv(dt):
    if dt == 'LNRGR':
        return np.array([[1,0]])
    elif dt == 'LNRDC':
        return np.array([[-1,0]])
    elif dt == 'PEXGR':
        return np.asanyarray([[1,1]])
    elif dt == 'NEXGR':
        return np.asanyarray([[1,-1]])
    elif dt == 'PEXDC':
        return np.asanyarray([[-1,-1]])
    elif dt == 'NEXDC':
        return np.asanyarray([[-1,1]])
    elif dt == 'SSHGR':
        return np.asanyarray([[1,1],[1,-1]])
    elif dt == 'SSHDC':
        return np.asanyarray([[-1,-1],[-1,1]])
    elif dt == 'GR1D1':
        return np.asanyarray([[1,-1],[-1,-1]])
    elif dt == 'GR1D2':
        return np.asanyarray([[1,-1],[-1,-1],[-1,1]])
    elif dt == 'GR2D1':
        return np.asanyarray([[1,1],[1,-1],[-1,-1]])
    elif dt == 'GR2D2':
        return np.asanyarray([[1,1],[1,-1],[-1,-1],[-1,1]])
    elif dt == 'H1':
        return np.asanyarray([[-1,-1],[-1,1],[-1,-1],[-1,1]])
    elif dt == 'H2':
        return np.asanyarray([[-1,-1],[-1,1],[-1,-1]])
    elif dt == 'H3':
        return np.asanyarray([[1,1],[1,-1],[1,1],[1,-1]])
    elif dt == 'H4':
        return np.asanyarray([[1,1],[1,-1],[1,1]])
    elif dt == 'H5':
        return np.asanyarray([[1,-1],[1,1],[1,-1]])
    elif dt == 'H6':
        return np.asanyarray([[1,-1],[1,1]])
    
def numerize_class(vec):
    new_vec =[]
    for dt in vec:  
        if dt == 'LNRGR':
            new_vec.append(1)
        elif dt == 'LNRDC':
            new_vec.append(2)
        elif dt == 'PEXGR':
            new_vec.append(3)
        elif dt == 'NEXGR':
            new_vec.append(4)
        elif dt == 'PEXDC':
            new_vec.append(5)
        elif dt == 'NEXDC':
            new_vec.append(6)
        elif dt == 'SSHGR':
            new_vec.append(7)
        elif dt == 'SSHDC':
            new_vec.append(8)
        elif dt == 'GR1D1':
            new_vec.append(9)
        elif dt == 'GR1D2':
            new_vec.append(10)
        elif dt == 'GR2D1':
            new_vec.append(11)
        elif dt == 'GR2D2':
            new_vec.append(12)
        elif dt == 'H1':
            new_vec.append(13)
        elif dt == 'H2':
            new_vec.append(14)
        elif dt == 'H3':
            new_vec.append(15)
        elif dt == 'H4':
            new_vec.append(16)
        elif dt == 'H5':
            new_vec.append(17)
        elif dt == 'H6':
            new_vec.append(18)
    return new_vec

def normalize_data(data):
    return (data-min(data))/(max(data)-min(data))


def normalize_dataframe(dataframe):
    normalized_dataframe = np.zeros(dataframe.shape)
    for i in range(dataframe.shape[0]):
        normalized_dataframe[i,:] = normalize_data(dataframe[i,:])
    return normalized_dataframe

def distance_dtw(sample1, sample2, wSlopeError=1, wCurvatureError=1):
    dtw = np.zeros([sample1.shape[0] + 1, sample2.shape[0] + 1])
    dtw[:, 0] = 1000
    dtw[0, :] = 1000
    dtw[0, 0] = 0
    for i in range(sample1.shape[0]):
        for j in range(sample2.shape[0]):
            cost = local_dist(sample1[i], sample2[j], wSlopeError, wCurvatureError)
            dtw[i + 1, j + 1] = cost + min([dtw[i + 1, j], dtw[i, j + 1], dtw[i, j]])
#             print i+1, j+1, dtw[i + 1, j + 1]
    max_len = max(sample1.shape[0], sample2.shape[0])
    min_len = min(sample1.shape[0], sample2.shape[0])
    return dtw[sample1.shape[0], sample2.shape[0]] /min_len

def local_dist(x, y, wDim1=1, wDim2=1):
    slope_e = np.square(x[0] - y[0])
    curv_e = np.square(x[1] - y[1])
    return  wDim1 * slope_e + wDim2 * curv_e

def num_after_point(x):
    s = str(x)
    if not '.' in s:
        return 0
    return len(s) - s.index('.') - 1

def find_data_decimal(ts):
    max_dec = -10000
    for i in ts:
        temp = num_after_point(i)
        if temp>max_dec:
            max_dec=temp
    return max_dec

def generate_value_property(ts,dec):
    relative_value = pow(10, -dec)
    value_property = []
    for i in ts:
        if abs(i)>relative_value:
            value_property.append(1)
        elif i ==0 :
            value_property.append(1)
        else:
            value_property.append(0)
    return value_property

def normalize(ts):
    new_ts=[]
    for i in range(len(ts)):
        new_ts.append((ts[i]-min(ts))/max(ts))
    return new_ts

def difference(ts,dec):
    differ=[]
    for i in range(len(ts)-1):
        differ.append(round(ts[i+1]-ts[i],dec))
    return differ

def merge(fv):
    full_fv = []
    for i in fv:
        if full_fv==[]:
            full_fv.append(i)
        else:
            current_fv = full_fv[len(full_fv)-1]
            if (i[0] == current_fv[0]) and (i[1] == current_fv[1]):
                full_fv[len(full_fv)-1] = [current_fv[0],current_fv[1],current_fv[2],i[3],i[3]-current_fv[2]+1,1]
            else:
                full_fv.append(i)
    return np.asarray(full_fv)

def transform_sign(fv):
    for i in range(fv.shape[0]):
        for j in range(2):
            if fv[i,j]>0:
                fv[i,j] = 1
            elif fv[i,j]<0:
                fv[i,j] = -1
            else:
                fv[i,j] = 0
    return fv

def saturation_alternation(v):
    if v==0:
        return -1
    elif v == -1:
        return 0
    
def saturation_check(fv):
    temp_fv = []
    new_fv=[]
    sequence_controller = 1000
    for i in range(fv.shape[0]):
        if len(temp_fv)<1:
            if fv[i,1]==-1 or fv[i,1]==0:
                temp_fv.append(fv[i,:])
                sequence_controller = fv[i,0]  
            else:
                new_fv.append(fv[i,:])
        else:
            if fv[i,0] == sequence_controller and fv[i,1]==saturation_alternation(temp_fv[len(temp_fv)-1][1]):
                temp_fv.append(fv[i,:])
            else:
                if len(temp_fv)>1:
                    new_fv.append(np.asarray([sequence_controller,-1]))
                    new_fv.append(fv[i,:])
                    temp_fv=[]
                else:
                    new_fv = new_fv+temp_fv
                    temp_fv=[]
                    new_fv.append(fv[i,:])
    if len(temp_fv)>0:
        if len(temp_fv)>1:
            new_fv.append([sequence_controller,-1])
            temp_fv=[]
        else:
            new_fv = new_fv+temp_fv
            temp_fv=[]
            
    if np.array_equal(new_fv[len(new_fv)-1],np.asarray([0,0])):
        del new_fv[-1]       
    return np.asarray(new_fv)

def saturation_alternation2(v):
    if v==0:
        return 1
    elif v == 1:
        return 0
    
def saturation_check2(fv):
    temp_fv = []
    new_fv=[]
    sequence_controller = 1000
    for i in range(fv.shape[0]):
        if len(temp_fv)<1:
            if fv[i,1]==1 or fv[i,1]==0:
                temp_fv.append(fv[i,:])
                sequence_controller = fv[i,0]  
            else:
                new_fv.append(fv[i,:])
        else:
            if fv[i,0] == sequence_controller and fv[i,1]==saturation_alternation2(temp_fv[len(temp_fv)-1][1]):
                temp_fv.append(fv[i,:])
            else:
                if len(temp_fv)>1:
                    new_fv.append(np.asarray([sequence_controller,1]))
                    new_fv.append(fv[i,:])
                    temp_fv=[]
                else:
                    new_fv = new_fv+temp_fv
                    temp_fv=[]
                    new_fv.append(fv[i,:])
    if len(temp_fv)>0:
        if len(temp_fv)>1:
            new_fv.append([sequence_controller,1])
            temp_fv=[]
        else:
            new_fv = new_fv+temp_fv
            temp_fv=[]
        
    if np.array_equal(new_fv[len(new_fv)-1],np.asarray([0,0])):
        del new_fv[-1] 

    return np.asarray(new_fv)


def saturation_process_check(fv):
    n_fv=[]
    for i in fv:
        if (np.array_equal(i, [0,1])==False) and (np.array_equal(i, [0,-1])==False):
            n_fv.append(i)
    
    nn_fv=[]
    for k in range(len(n_fv)):
        if np.array_equal(n_fv[k], [0,0])==False:
            nn_fv.append(n_fv[k])
    return np.asarray(nn_fv)

def create_fv_updated(ts):
    ts = ts.tolist()
    dec = find_data_decimal(ts)
    
    
    trend = difference(ts,dec)
    trend_property = generate_value_property(trend,dec)
    curvature = difference(trend,dec)
    curvature_property = generate_value_property(curvature,dec)
    
    complete_property=[]
    for i in range(len(curvature_property)):
        if (trend_property[i]==1) and (curvature_property[i]==1):
            complete_property.append(1)
        else:
            complete_property.append(0)

    #===========================================================================
    # print trend_property
    # print curvature_property
    # print trend
    # print curvature
    #===========================================================================
    fv = np.zeros((len(curvature),6))
    fv[:,0]=trend[0:(len(trend)-1)]
    fv[:,1]=curvature[0:(len(trend)-1)]
    fv[:,2]=range(len(trend)-1)
    fv[:,3]=range(1,len(trend))
    fv[:,4]=[1]*len(curvature)
    fv[:,5]=complete_property
    
    fv=fv[fv[:,5]==1]
    fv = transform_sign(fv)
    return merge(fv)
    #raw_input()


def create_fv_updated_debug(ts):
    ts = ts.tolist()
    dec = find_data_decimal(ts)
    
    
    trend = difference(ts,dec)
    trend_property = generate_value_property(trend,dec)
    curvature = difference(trend,dec)
    curvature_property = generate_value_property(curvature,dec)
    
    complete_property=[]
    for i in range(len(curvature_property)):
        if (trend_property[i]==1) and (curvature_property[i]==1):
            complete_property.append(1)
        else:
            complete_property.append(0)

    #===========================================================================
    # print trend_property
    # print curvature_property
    # print trend
    # print curvature
    #===========================================================================
    fv = np.zeros((len(curvature),6))
    fv[:,0]=trend[0:(len(trend)-1)]
    fv[:,1]=curvature[0:(len(trend)-1)]
    fv[:,2]=range(len(trend)-1)
    fv[:,3]=range(1,len(trend))
    fv[:,4]=[1]*len(curvature)
    fv[:,5]=complete_property
    
    fv=fv[fv[:,5]==1]
    fv = transform_sign(fv)
    return merge(fv)

def import_pandas_hybrid_data():
    relPathFileFolder = 'C:/Users/sesdyn12/Desktop'
    inputFileName='Hybrid-S'
    book = open_workbook(relPathFileFolder+'/'+inputFileName+'.xlsx')
    new_data = pd.DataFrame()
    clss=[]
    for k in ["H1","H2","H3","H4","H5","H6"]:
        
        sheet_data=book.sheet_by_name(k)
        noRuns = sheet_data.nrows-1
        for i in range(noRuns):
                entry = []
                if i == 0:
                    data =[sheet_data.row_values(i+1)]
                else:
                    data.append(sheet_data.row_values(i+1))
        new_data = new_data.append(pd.DataFrame(np.array(data)),ignore_index=True)
        clss = clss+[k]*noRuns
    new_data = new_data.rename(columns = {0:'Id'})
    new_data["Class"] = clss
    new_data['Source']=["StellaFreeFloat"]*new_data.shape[0]
    return new_data

def feature_matrix_construct(data_values):
    all_fv=[] 
    for i in range(data_values.shape[0]):
        fv = create_fv_updated(data_values[i])[:,0:2]
        fv = saturation_check(fv)
        fv = saturation_check2(fv)
        fv = saturation_process_check(fv)
        all_fv.append(fv)
    
    return all_fv

def create_distance_matrix(all_fv):
    dtw_all = np.zeros((len(all_fv),len(all_fv)))
    for i in range(len(all_fv)):
        for j in range(len(all_fv)):
            dtw_all[i,j] = distance_dtw(all_fv[i],all_fv[j])
    return dtw_all

def cluster(data_values,all_fv,plot_options=[True,True],save_plot=False,write=False,adress="../"):
    dtw_all = create_distance_matrix(all_fv)
    DB = DBSCAN(metric="precomputed").fit(dtw_all)
    clusters= DB.labels_ 
    
    if plot_options[0] == True:
        if plot_options[1] == True:
            data = pd.DataFrame(normalize_dataframe(data_values))
        else:
            data = pd.DataFrame(data_values)
        data['Clusters']=clusters
        print clusters
        f_i = 0
        for i in np.unique(data['Clusters']):
            f_i=f_i+1
            plt.figure(f_i)
            plt.title("Cluster "+str(i))

            plt.plot(data[data['Clusters']==i].iloc[:,0:(data.shape[1]-1)].as_matrix().transpose(),color="b")
            if save_plot == True:
                plt.savefig(adress+'Cluster_'+str(i)+'.png')
        plt.show()
    if write == True:
        clusters = np.asarray(clusters)
        clusters.astype(int)
        np.savetxt(adress+"clusters.txt", clusters,fmt='%i',delimiter=",")
    return clusters

def read_data(directory='../data.xlsx'):
    book = open_workbook(directory)
    new_data=pd.DataFrame()
    for k in book.sheet_names():
        sheet_data=book.sheet_by_name(k)
        noRuns = sheet_data.nrows
        for i in range(noRuns-1):
                entry = []
                if i == 0:
                    data =[sheet_data.row_values(i+1)]
                else:
                    data.append(sheet_data.row_values(i+1))
        new_data = new_data.append(pd.DataFrame(np.array(data)),ignore_index=True)
    new_data = new_data.rename(columns = {0:'Id'})
    return new_data


dirn=os.path.dirname(os.path.realpath(__file__))
data = read_data(dirn+'/test.xlsx')
fv = feature_matrix_construct(data.iloc[:,1:(data.shape[1])].as_matrix())
cluster(data.iloc[:,1:(data.shape[1])].as_matrix(),fv,adress=dirn+'/',save_plot=True,write=True)