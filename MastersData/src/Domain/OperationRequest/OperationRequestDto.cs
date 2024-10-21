using System;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.StaffMembers;



namespace DDDSample1.Domain.OperationRequest
{


    public class OperationRequestDto
    {
        public Guid Id { get; set; }

        public string DeadLineDate { get; set; }

        public string Priority { get; set; }

//        public PatientId? PatientId { get; set; }

  //      public OperationTypeId? OperationTypeId { get; set; }

          //public StaffId? DoctorId { get; set; }

        public String PatientId { get; set; }
        public String OperationTypeId { get; set; }
        public String DoctorId { get; set;}

/*
        public OperationRequestDto(Guid id, string deadLineDate, string priority, PatientId patientId, OperationTypeId operationTypeId, StaffId doctorId)
        {
            this.Id = id;
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;
            this.DoctorId = doctorId;
        }
  */      

        public OperationRequestDto(Guid id, string deadLineDate, string priority, String patientId, String operationTypeId, String doctorId)
        {
            this.Id = id;
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;
            this.DoctorId = doctorId;
        }



    }






}