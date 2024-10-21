
using System;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.StaffMembers;



namespace DDDSample1.Domain.OperationRequestLoggers
{


    public class OperationRequestLoggerDto
    {
        public string OperationRequestLoggerId { get; private set; }
        public string OperationRequestId { get; private set; }
        public string Priority { get; private set; }
        public string OperationTypeId { get; private set; }
        public string DoctorId { get; private set; }
        public string DeadLineDate { get; private set; } 

        public DateTime CreatedAt { get; private set; } = DateTime.Now;

        public string loggerType { get; private set; }

       

        public OperationRequestLoggerDto(string operationRequestLoggerId,string deadLineDate, string priority, string operationTypeId, string doctorId, string operationRequestId, string loggerType)
        {
            this.OperationRequestLoggerId = operationRequestLoggerId;
            this.OperationRequestId = operationRequestId;
            this.Priority = priority;
            this.DeadLineDate = deadLineDate;
            this.OperationTypeId = operationTypeId;
            this.DoctorId = doctorId;
            this.loggerType = loggerType;
            this.CreatedAt = DateTime.Now;
        }




    }






}