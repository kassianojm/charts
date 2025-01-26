# Spark Analysis Scripts

This repository contains a set of R scripts designed to analyze and visualize Spark cluster performance and resource utilization.

---

## **Scripts Overview**

### `Cache-mean.R`
This script calculates the average (AVG) of data distributed across Spark Executors' memory. The solution is optimized to minimize memory usage, ensuring a global memory range between **8GB (minimum)** and **20GB (maximum)** across all executors.

#### **Output:**
- Console output

---

### `STD-DEV(Grenoble).R`
This script measures the following performance metrics for Spark processing:
- **AVG Throughput (Th):** Average throughput in MBps.
- **AVG Processing Time (PT):** Average Spark processing time in milliseconds (ms).
- **AVG Scheduling Delay (SD):** Average scheduling delay in milliseconds (ms).
- **AVG Records Processed (Rec):** Average number of processed records per second (s).

#### **Output File:**
- `avg_statistics.csv` (saved in the `log` folder)

---

### `Adapt-grenoble-auto.R`
This script dynamically generates performance and resource utilization charts based on input logs.

#### **Input Logs:**
1. **GC Status:**
   - `DRIVER.LOG` (Driver logs)  
   - `EXECUTOR[2-9].LOG` (Executor logs)  

2. **Resource Logs:**
   - **Dstat Resources:**  
     Logs generated using [Dstat Monitor](https://github.com/mvneves/dstat-monitor).  
   - **MQ Resources:**  
     - `MQRESOURCES[1-8].CSV`  
     - `RESOURCES[1-9].CSV`  
       - 1 Driver  
       - 2–8 Executors  

3. **Spark API Data:**
   - `batch_infos.csv`  

4. **MQ Middleware Logs (MQ-based Solution):**
   - `LOG[1-8].CSV`  

#### **Output Charts:**
- **Cacheglobal.pdf:** Total data distributed across Spark Executors' memory.  
- **Delay.pdf:** Spark processing time and scheduling delay over time.  
- **GlobalMemUtilization.pdf:** Total cluster memory utilization over time.  
- **MemUtilization.pdf:** Memory usage per executor over time.  
- **receivers.pdf:** Executor receiver throughput over time.  
- **loss.pdf:** Throughput loss over time.  

---

## **Folder Structure**

