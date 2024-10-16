using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Xml;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage;

namespace DDDSample1.Domain.Patient
{
    public class PatientService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPatientRepository _patientRepository;
        private readonly IEmailSender _emailSender;

        public PatientService(IUnitOfWork unitOfWork, IPatientRepository patientRepository, IEmailSender emailSender)
        {
            _unitOfWork = unitOfWork;
            _patientRepository = patientRepository;
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

            FullName fullName = new FullName(dto.FullName);
            DateOfBirth dateOfBirth = new DateOfBirth(DateTime.Parse(dto.DateOfBirth));
            Email email = new Email(dto.Email);
            PhoneNumber phoneNumber = new PhoneNumber(dto.PhoneNumber);

            EmergencyContact emergencyContact = null;
            if (!string.IsNullOrWhiteSpace(dto.EmergencyContact))
            {
                emergencyContact = new EmergencyContact(dto.EmergencyContact);
            }

            MedicalRecordNumber medicalRecordNumber = MedicalRecordNumberGenerator.GenerateMedicalRecordNumber();

            var patient = new Patient(
                fullName,
                dateOfBirth,
                email,
                phoneNumber,
                emergencyContact,
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

            string email = patient.Email.email;

            if (dto.FullName != null)
            {
                FullName fullName = new FullName(dto.FullName);
                patient.ChangeFullName(fullName);
            }

            if (dto.Email != null)
            {
                bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
                if (!emailIsUnique)
                {
                    throw new BusinessRuleValidationException("Email already exists");
                }
                Email newEmail = new Email(dto.Email);
                patient.ChangeEmail(newEmail);
            }

            if (dto.PhoneNumber != null)
            {
                bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
                if (!phoneNumberIsUnique)
                {
                    throw new BusinessRuleValidationException("Phone Number already exists");
                }
                PhoneNumber phoneNumber = new PhoneNumber(dto.PhoneNumber);
                patient.ChangePhoneNumber(phoneNumber);
            }

            if (dto.MedicalConditions != null)
            {
                MedicalConditions medicalConditions = new MedicalConditions(dto.MedicalConditions);
                patient.ChangeMedicalConditions(medicalConditions);
            }

            await _unitOfWork.CommitAsync();

            if (dto.Email != null || dto.PhoneNumber != null)
            {
                await _emailSender.SendEmailAsync("Your profile has been updated. If you did not make this change, please contact support immediately.", email, "Profile Update Notification");
            }

            return PatientMapper.ToDto(patient);
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


    }
}
