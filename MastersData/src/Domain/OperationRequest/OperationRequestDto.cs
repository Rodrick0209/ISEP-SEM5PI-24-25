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


        public String PatientId { get; set; }
        public String OperationTypeId { get; set; }
        public String DoctorThatRequestedId { get; set;}

        public String DoctorThatWillPerformId { get; set; }


      

        public OperationRequestDto(Guid id, string deadLineDate, string priority, String patientId, String operationTypeId, String doctorThatRequestedId, String doctorThatWillPerformId)
        {
            this.Id = id;
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
            this.PatientId = patientId;
            this.OperationTypeId = operationTypeId;
            this.DoctorThatWillPerformId = doctorThatWillPerformId;
            this.DoctorThatRequestedId = doctorThatRequestedId;
        }
       


    }






}