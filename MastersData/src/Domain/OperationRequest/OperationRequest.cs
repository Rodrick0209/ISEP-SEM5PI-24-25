using System;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;
using System.Collections.Generic;



namespace DDDSample1.Domain.OperationRequest
{

    public class OperationRequest : Entity<OperationRequestId>,IAggregateRoot
    {

        public DeadLineDate deadLineDate { get; private set; }
        public Priority priority { get; private set; }
        public String patientId {get; private set;}
        public String operationTypeId{get; private set;}
        public String doctorThatRequestedId{get; private set;}
        public String doctorThatWillPerformId{get; private set;}

        public OperationRequestStatus status { get; private set; }

        public StaffAssignedSurgery staffAssignedSurgery { get; private set; }


        private OperationRequest()
        {
        }
        public OperationRequest(string deadLineDate, string priority, String patientId, String operationTypeId, String doctorThatRequestedId, String doctorThatWillPerformId)
        {
            this.Id = new OperationRequestId(Guid.NewGuid());
            this.priority = new Priority(priority);
            this.deadLineDate = new DeadLineDate(deadLineDate);
            this.patientId = patientId;
            this.operationTypeId = operationTypeId;
            this.doctorThatRequestedId = doctorThatRequestedId;
            this.doctorThatWillPerformId = doctorThatWillPerformId;
            this.status = OperationRequestStatus.Waiting;
            this.staffAssignedSurgery = new StaffAssignedSurgery(new List<StaffId>(), new List<StaffId>());
        }

        public void AssignStaffToAnesthesyPhase(List<StaffId> staffId)
        {
            this.staffAssignedSurgery.staffAnesthesyPhase.Clear();
            this.staffAssignedSurgery.staffAnesthesyPhase.AddRange(staffId);
        }

        public void AssignStaffToSurgeryPhase(List<StaffId> staffId)
        {
            this.staffAssignedSurgery.staffSurgeryPhase.Clear();
            this.staffAssignedSurgery.staffSurgeryPhase.AddRange(staffId);
        }


        public List<StaffId> getStaffAnesthesyPhase()
        {
            return this.staffAssignedSurgery.staffAnesthesyPhase;
        }   

        public List<StaffId> getStaffSurgeryPhase()
        {
            return this.staffAssignedSurgery.staffSurgeryPhase;
        }
       

        public void ChangeDeadLineDate(string deadLineDate)
        {
            this.deadLineDate = new DeadLineDate(deadLineDate);
        }


        

        public void ChangePriority(string priority)
        {
            this.priority = new Priority(priority);
        }

        public void Accepted()
        {
            this.status = OperationRequestStatus.Accepted;
        }

        public void Cancelled()
        {
            this.status = OperationRequestStatus.Waiting;
        }
        public bool IsAvailable(OperationRequestStatus status)
        {
            if(status == OperationRequestStatus.Waiting)
            {
                return true;
            }

            return false;
        }	

       
    }







}