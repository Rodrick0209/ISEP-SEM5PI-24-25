using System;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.StaffMembers;



namespace DDDSample1.Domain.OperationRequest
{


    public class ChangeOperationRequestDto
    {
        public Guid Id { get; set; }

        public string DeadLineDate { get; set; }

        public string Priority { get; set; }

  
        public ChangeOperationRequestDto(Guid id, string deadLineDate, string priority)
        {
            this.Id = id;
            this.DeadLineDate = deadLineDate;
            this.Priority = priority;
        }



    }






}