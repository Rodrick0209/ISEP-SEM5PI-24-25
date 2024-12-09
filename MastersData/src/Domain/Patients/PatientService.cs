using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using System.Xml;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage;
using Microsoft.IdentityModel.Tokens;

namespace DDDSample1.Domain.Patients
{
    public class PatientService : IPatientService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPatientRepository _patientRepository;
        private readonly IPatientLoggerRepository _patientLoggerRepository;
        private readonly IEmailSender _emailSender;

        public PatientService(IUnitOfWork unitOfWork, IPatientRepository patientRepository, IPatientLoggerRepository patientLoggerRepository, IEmailSender emailSender)
        {
            _unitOfWork = unitOfWork;
            _patientRepository = patientRepository;
            _patientLoggerRepository = patientLoggerRepository;
            _emailSender = emailSender;
        }

        public async Task<PatientDto> CreateAsync(CreatingPatientProfileDto dto)
        {

            if (string.IsNullOrWhiteSpace(dto.FirstName))
            {
                throw new BusinessRuleValidationException("Invalid first name");
            }

            if (string.IsNullOrWhiteSpace(dto.LastName))
            {
                throw new BusinessRuleValidationException("Invalid last name");
            }

            bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
            bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            var lastPatientRegisterInMonth = await _patientRepository.GetLastPatientRegisteredInMonthAsync();

            string medicalRecordNumber = MedicalRecordNumberGenerator.GenerateMedicalRecordNumber(lastPatientRegisterInMonth);

            var patient = new Patient(
                dto.FullName,
                dto.DateOfBirth,
                dto.Gender,
                dto.Email,
                dto.PhoneNumber,
                dto.Street,
                dto.PostalCode,
                dto.City,
                dto.Country,
                dto.EmergencyContactName,
                dto.EmergencyContactEmail,
                dto.EmergencyContactPhoneNumber,
                medicalRecordNumber
            );

            

            await _patientRepository.AddAsync(patient);
            await _unitOfWork.CommitAsync();

            return PatientMapper.ToDto(patient);
        }


        public async Task<PatientDto> UpdateAsync(EditingPatientProfileDto dto)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(dto.MedicalRecordNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            LogChanges(patient, "update");

            string email = patient.Email.email;

            if (!string.IsNullOrWhiteSpace(dto.Name))
            {
                patient.ChangeFullName(dto.Name);
            }

            if (!string.IsNullOrWhiteSpace(dto.Email))
            {
                bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
                if (!emailIsUnique && patient.Email.email!=dto.Email)
                {
                    throw new BusinessRuleValidationException("Email already exists");
                }
                patient.ChangeEmail(dto.Email);
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumber))
            {
                bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
                if (!phoneNumberIsUnique && patient.PhoneNumber.phoneNumber!=dto.PhoneNumber)
                {
                    throw new BusinessRuleValidationException("Phone Number already exists");
                }
                patient.ChangePhoneNumber(dto.PhoneNumber);
            }

            if (!string.IsNullOrWhiteSpace(dto.Street))
            {
                patient.Address.ChangeStreet(dto.Street);
            }

            if (!string.IsNullOrWhiteSpace(dto.PostalCode))
            {
                patient.Address.ChangePostalCode(dto.PostalCode);
            }

            if (!string.IsNullOrWhiteSpace(dto.City))
            {
                patient.Address.ChangeCity(dto.City);
            }

            if (!string.IsNullOrWhiteSpace(dto.Country))
            {
                patient.Address.ChangeCountry(dto.Country);
            }


            await _unitOfWork.CommitAsync();

            if (dto.Email != null || dto.PhoneNumber != null)
            {
                string subject = "Profile Update Notification";
                string body = $"Dear User,<br><br>" +
                              $"We wanted to let you know that changes have been made to your profile. The updated details may include your email address or phone number.<br><br>" +
                              $"If you made these changes, no further action is required.<br>" +
                              $"However, if you did not make this update, please contact our support team immediately to ensure the security of your account.<br><br>" +
                              $"Best regards,<br><br>" +
                              $"[System Appointment and Resource Management]";
                await _emailSender.SendEmailAsync(body, email, subject);
            }

            return PatientMapper.ToDto(patient);
        }

        public async Task DeleteAsync(string medicalRecordNumber)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(medicalRecordNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            LogChanges(patient, "delete");

            _patientRepository.Remove(patient);

            await _unitOfWork.CommitAsync();
        }

        public async Task<List<ViewPatientDto>> SearchAsync(SearchFiltersDto dto)
        {
            var patients = new List<Patient>();
            if (string.IsNullOrWhiteSpace(dto.MedicalRecordNumber) && string.IsNullOrWhiteSpace(dto.Name) && string.IsNullOrWhiteSpace(dto.Email) && string.IsNullOrWhiteSpace(dto.DateOfBirth))
            {
                patients = await _patientRepository.GetAllAsync();
            }
            else
            {
                if (!string.IsNullOrWhiteSpace(dto.DateOfBirth))
                {
                    dto.DateOfBirth = dto.DateOfBirth.Substring(0, 10);
                }
                patients = await _patientRepository.GetByFiltersAsync(dto.MedicalRecordNumber, dto.Name, dto.Email, dto.DateOfBirth);
            }

            List<ViewPatientDto> listDto = patients.ConvertAll<ViewPatientDto>(pat => new ViewPatientDto
            {
                MedicalRecordNumber = pat.MedicalRecordNumber._medicalRecordNumber,
                Name = pat.FullName.fullName,
                Email = pat.Email.email,
                DateOfBirth = pat.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd")
            });

            return listDto;
        }



        public async Task<List<PatientDto>> GetAllAsync()
        {
            var list = await _patientRepository.GetAllAsync();

            List<PatientDto> listDto = list.ConvertAll<PatientDto>(pat => PatientMapper.ToDto(pat));

            return listDto;
        }

        public async Task<PatientDto> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(medicalRecordNumber);

            return patient == null ? null : PatientMapper.ToDto(patient);
        }

        public async Task<PatientDto> GetByIdAsync(PatientId id)
        {
            var patient = await _patientRepository.GetByIdAsync(id);

            return patient == null ? null : PatientMapper.ToDto(patient);
        }

        private async Task<bool> ValidateEmailIsUnique(string email)
        {
            var existingPatient = await _patientRepository.GetByEmailAsync(email);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> ValidatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingPatient = await _patientRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private void LogChanges(Patient patient, string typeOfChange)
        {
            var patientLogger = new PatientLogger(
                patient.Id,
                patient.MedicalRecordNumber._medicalRecordNumber,
                typeOfChange
            );

            _patientLoggerRepository.AddAsync(patientLogger);
        }

        public async Task<PatientDto> GetByEmailAsync(string email)
        {
            var patient = await _patientRepository.GetByEmailAsync(email);

            return patient == null ? null : PatientMapper.ToDto(patient);
        }
    }
}
