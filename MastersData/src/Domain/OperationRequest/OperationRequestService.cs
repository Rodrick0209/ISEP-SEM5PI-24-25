using System.Threading.Tasks;
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Domain.Utils;
using Microsoft.AspNetCore.Mvc;
using Microsoft.VisualBasic;
using System.Linq;



namespace DDDSample1.Domain.OperationRequest
{
    public class OperationRequestService : IOperationRequestService
    {

        private readonly IUnitOfWork _unitOfWork;

        private readonly IOperationRequestRepository _repo;


        // private readonly IAppointmentRepository _appointmentRepository;

        private readonly IPatientRepository _patientRepository;

        private readonly IOperationTypeRepository _operationTypeRepository;

        private readonly IStaffRepository _staffRepository;

        private readonly IOperationRequestLoggerRepository _operationRequestLoggerRepository;

        public OperationRequestService(IUnitOfWork unitOfWork, IOperationRequestRepository repo, IOperationTypeRepository operationTypeRepository, IStaffRepository staffRepository, IPatientRepository patientRepository, IOperationRequestLoggerRepository operationRequestLoggerRepository)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
            this._operationTypeRepository = operationTypeRepository;
            this._staffRepository = staffRepository;
            this._patientRepository = patientRepository;
            this._operationRequestLoggerRepository = operationRequestLoggerRepository;
        }




        public async Task<OperationRequest> AddAsync(OperationRequest operationRequest)
        {
            await CheckOperationTypeIdAsync(operationRequest.operationTypeId);
            await CheckDoctorIdAsync(new StaffId(operationRequest.doctorThatWillPerformId));
            await CheckPatientAsync(new PatientId(operationRequest.patientId));

            await this._repo.AddAsync(operationRequest);

            await this._unitOfWork.CommitAsync();

            return operationRequest;

        }

        public async Task<OperationRequest> UpdateAsync(ChangeOperationRequestDto dto, string doctorThatWantsToUpdateEmail)
        {
            var op = await this._repo.GetByIdAsync(new OperationRequestId(dto.Id));
            Email email = new Email(doctorThatWantsToUpdateEmail);
            var doctorThatRequestedId = email.getFirstPartOfEmail();

            if (op == null)
            {
                throw new BusinessRuleValidationException("Operation Request not found");
            }

            if (!op.doctorThatRequestedId.Equals(doctorThatRequestedId))
            {
                throw new BusinessRuleValidationException("Doctor not allowed to update this operation request");
            }

            var objetoLogger = LogObjectCreate(op, LoggerTypeOfChange.Update);

            bool hasChanges = false;

            if (!string.IsNullOrWhiteSpace(dto.Priority) && !op.priority.priority.Equals(dto.Priority))
            {
                op.ChangePriority(dto.Priority);
                hasChanges = true;
            }

            if (!string.IsNullOrWhiteSpace(dto.DeadLineDate) && !op.deadLineDate.deadLineDate.Equals(dto.DeadLineDate))
            {
                op.ChangeDeadLineDate(dto.DeadLineDate);
                hasChanges = true;
            }

            if (hasChanges)
            {
                await this._operationRequestLoggerRepository.AddAsync(objetoLogger);
            }

            await this._unitOfWork.CommitAsync();
            return op;
        }


        private OperationRequestLogger LogObjectCreate(OperationRequest operationRequest, LoggerTypeOfChange typeOfChange)
        {
            return new OperationRequestLogger(operationRequest.deadLineDate.deadLineDate, operationRequest.priority.priority, operationRequest.operationTypeId, operationRequest.doctorThatWillPerformId, operationRequest.doctorThatRequestedId, operationRequest.Id.AsString(), typeOfChange.ToString());
        }



        public async Task<OperationRequest> DeleteAsync(OperationRequestId id)
        {

            var op = await this._repo.GetByIdAsync(id);

            if (op == null)
                throw new BusinessRuleValidationException("Operation Request not found");



            if (op.status.Equals(OperationRequestStatus.Accepted))
                throw new BusinessRuleValidationException("Operation Request is scheduled and cannot be deleted");


            this._repo.Remove(op);
            await this._unitOfWork.CommitAsync();

            return op;

        }





        public async Task<OperationRequest> GetByIdAsync(OperationRequestId id)
        {

            var op = await this._repo.GetByIdAsync(id);
            if (op == null)
                return null;

            return op;

        }


        private async Task<OperationType> CheckOperationTypeIdAsync(string operationTypeId)
        {
            try
            {
                var id = new OperationTypeId(operationTypeId);
                var opType = await this._operationTypeRepository.GetByIdAsync(id);

                if (opType == null)
                {
                    throw new BusinessRuleValidationException("Operation Type not found");
                }

                return opType;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Operation Type Not Found");
            }
        }





        private async Task<Staff> CheckDoctorIdAsync(StaffId doctorId)
        {
            var staff = await this._staffRepository.GetByIdAsync(doctorId);

            if (staff == null)
                throw new BusinessRuleValidationException("Doctor not found");

            if (!staff.Category.ToString().Equals("Doctor"))
                throw new BusinessRuleValidationException("Staff is not a Doctor");


            return staff;
        }

        public async Task<List<OperationRequest>> GetAllAsync()
        {
            return await this._repo.GetAllAsync();
        }

        private async Task<Patient> CheckPatientAsync(PatientId id)
        {
            var patient = await this._patientRepository.GetByIdAsync(id);
            if (patient == null)
                throw new BusinessRuleValidationException("Patient not found");
            return patient;
        }


        public async Task<List<OperationRequestDto>> GetOperationRequestsWithFilters(OperationRequestFilterDto filters, string doctorIdEmail)
        {
            List<OperationRequest> query = null;

            if (!string.IsNullOrWhiteSpace(filters.MedicalRecordNumber))
            {
                var patient = await _patientRepository.GetByMedicalRecordNumberAsync(filters.MedicalRecordNumber);

                if (patient != null)
                {
                    query = await _repo.GetOperationRequestsByPatientId(patient.Id.AsString());
                }

                if (patient == null)
                {
                    return new List<OperationRequestDto>();
                }

            }
            if (!string.IsNullOrWhiteSpace(filters.PatientName))
            {
                var patients = await _patientRepository.GetByNameAsync(filters.PatientName);

                if (patients != null && patients.Any())
                {
                    List<OperationRequest> matchingRequests = new List<OperationRequest>();

                    foreach (var patient in patients)
                    {
                        if (query == null)
                        {
                            var patientRequests = await _repo.GetOperationRequestsByPatientId(patient.Id.AsString());
                            matchingRequests.AddRange(patientRequests);
                        }
                        else
                        {
                            var patientRequests = query.FindAll(or => or.patientId.Equals(patient.Id.AsString()));
                            matchingRequests.AddRange(patientRequests);
                        }
                    }

                    query = matchingRequests;
                }
                else
                {
                    return new List<OperationRequestDto>();
                }
            }


            if (query == null && !string.IsNullOrWhiteSpace(doctorIdEmail))
            {
                string doctorId = new Email(doctorIdEmail).getFirstPartOfEmail();
                query = await _repo.GetOperationRequestsByDoctorIdRequested(doctorId);
                Console.WriteLine("Size query: " + query.Count);
            }

            if (query == null || query.Count == 0)
            {
                return new List<OperationRequestDto>();
            }



            if (!string.IsNullOrWhiteSpace(filters.OperationType))
            {
                var operationType = await _operationTypeRepository.GetByNameAsync(filters.OperationType);
                if (operationType == null)
                {
                    return new List<OperationRequestDto>();
                }
                query = query.FindAll(or => or.operationTypeId.Equals(operationType.Id.AsString()));
            }



            if (filters.StartDate.HasValue)
            {
                query = query.FindAll(or =>
                {
                    DateTime deadLineDateTime = DateTime.ParseExact(or.deadLineDate.deadLineDate, "yyyy-MM-dd", null);
                    return deadLineDateTime >= filters.StartDate.Value;
                });
            }

            if (filters.EndDate.HasValue)
            {
                query = query.FindAll(or =>
                {
                    DateTime deadLineDateTime = DateTime.ParseExact(or.deadLineDate.deadLineDate, "yyyy-MM-dd", null);
                    return deadLineDateTime <= filters.EndDate.Value;
                });
            }


            //List<OperationRequestDto> result = query.ConvertAll(or => OperationRequestMapper.toDTO(or));
            List<OperationRequestDto> result = new List<OperationRequestDto>();
            if (query != null)
            {
                Console.WriteLine("Size query: " + query.Count);
                result = TransformOperationRequestsForUi(query);
                Console.WriteLine("Size result: " + result.Count);
            }


            return result;
        }


        public async Task<List<OperationRequestDto>> GetAllForUiAsync(string doctor)
        {
            Email email = new Email(doctor);
            var doctorThatRequestedId = email.getFirstPartOfEmail();
            List<OperationRequest> operationRequests = await this._repo.GetOperationRequestsByDoctorIdRequested(doctorThatRequestedId);
            
            List<OperationRequestDto> operationRequestDtos = new List<OperationRequestDto>();
            foreach (OperationRequest operationRequest in operationRequests)
            {
                var patientMedicalRecordNumber = _patientRepository.GetByIdAsync(new PatientId(operationRequest.patientId)).Result.MedicalRecordNumber._medicalRecordNumber;
                var operationTypeDesignation = _operationTypeRepository.GetByIdAsync(new OperationTypeId(operationRequest.operationTypeId)).Result.name;
                operationRequestDtos.Add(OperationRequestMapper.toDtoForUI(operationRequest, patientMedicalRecordNumber, operationTypeDesignation));
            }
            return operationRequestDtos;
        }

        public List<OperationRequestDto> TransformOperationRequestsForUi(List<OperationRequest> operationRequests)
        {
            
            List<OperationRequestDto> operationRequestDtos = new List<OperationRequestDto>();
            foreach (OperationRequest operationRequest in operationRequests)
            {
                var patientMedicalRecordNumber = _patientRepository.GetByIdAsync(new PatientId(operationRequest.patientId)).Result.MedicalRecordNumber._medicalRecordNumber;
                var operationTypeDesignation = _operationTypeRepository.GetByIdAsync(new OperationTypeId(operationRequest.operationTypeId)).Result.name;
                operationRequestDtos.Add(OperationRequestMapper.toDtoForUI(operationRequest, patientMedicalRecordNumber, operationTypeDesignation));
            }
            return operationRequestDtos;







        }
    }
}