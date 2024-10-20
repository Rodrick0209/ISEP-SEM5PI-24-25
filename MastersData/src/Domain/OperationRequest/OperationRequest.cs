using System;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;



namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequest : Entity<OperationRequestId>,IAggregateRoot
    {

        public DeadLineDate deadLineDate { get; private set; }
        public Priority priority { get; private set; }

        public PatientId patientId { get; private set; }

        public OperationTypeId operationTypeId { get; private set; }   

        public StaffId doctorId { get; private set; }

        private OperationRequest()
        {
        }

        public OperationRequest(string deadLineDate, string priority, PatientId patientId, OperationTypeId operationTypeId, StaffId doctorId)
        {
            this.Id = new OperationRequestId(Guid.NewGuid());
            this.priority = new Priority(priority);
            this.deadLineDate = new DeadLineDate(deadLineDate);
            this.patientId = patientId;
            this.operationTypeId = operationTypeId;
            this.doctorId = doctorId;
        }


        public void ChangeDeadLineDate(string deadLineDate)
        {
            this.deadLineDate = new DeadLineDate(deadLineDate);
        }


        public void ChangePriority(string priority)
        {
            this.priority = new Priority(priority);
        }

        public void ChangePatientId(PatientId patientId)
        {       
            this.patientId = patientId;
        }

        public void ChangeOperationTypeId(OperationTypeId operationTypeId)
        {
            this.operationTypeId = operationTypeId;
        }

        public void ChangeDoctorId(StaffId doctorId)
        {
            this.doctorId = doctorId;
        }


    }







}