

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Users;
using Microsoft.AspNetCore.Identity;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.Extensions.Configuration;
using DDDSample1.Domain.User;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion.Internal;

namespace DDDSample1.Domain.User
{
 
public  class UserService
{
    private readonly IUnitOfWork _unitOfWork;
    private readonly IUserRepository _repo;

    private readonly IConfiguration _configuration;
    
    private readonly IEmailSender _emailSender;
    

    public UserService(IUnitOfWork unitOfWork, IUserRepository repo,IConfiguration configuration,IEmailSender emailSender)
    {
        _unitOfWork = unitOfWork;
        _repo = repo;
        _configuration = configuration;
        _emailSender = emailSender;
    }

    

   public async Task<string> GetLogToken(LoginRequest request)
{
    if (request == null)
    {
        throw new Exception("Login request is null");
    }
    Console.WriteLine("TRACK ERROR 1");

    User? user = await _repo.GetByEmailAsync(request.Email);
    if (user == null)
    {
        throw new Exception("User not found");
    }

    bool result2 = user.checkIfAccountIsBlocked();
    Console.WriteLine("TRACK ERROR 2--- resultado > " + result2);

  
    if (result2)
    {
        throw new Exception("Account blocked, please wait or contact the admin");
    }


    PasswordHasher passwordHasher = new();
    var result = passwordHasher.VerifyPassword(user.password.password, request.Password);

    if (!result)
    {
        int maxAllowedFailCounter = int.Parse(_configuration["Auth:MaxAllowedFailCounter"]);
        int lockoutTime = int.Parse(_configuration["Auth:LockoutTime"]);
        int failsLeft = maxAllowedFailCounter - user.loginFailCounter.loginFailCounter - 1;
        user.IncreaseFailCounter(maxAllowedFailCounter, lockoutTime);
        await this._unitOfWork.CommitAsync();

        if(failsLeft == 0)
        {
            await this.SendEmailForAbusiveAccountAccess(user.email.email);
            throw new Exception("Account blocked, please wait or contact the admin");

        }
        throw new Exception("Invalid password you have " + failsLeft + " attempts left");
    }
    
    
    //Falta apenas notificar o admin que a conta bloqueou

    TokenProvider tokenProvider = new TokenProvider(_configuration);
    string token = tokenProvider.Create(user);

    return token; 
}


    //Metodo do serviço para registar o user no sistema
    public async Task<User> AddAsync(User user)
    {
                
            bool exists = await this._repo.CheckEmail(user.email.email);

            if(exists) throw new BusinessRuleValidationException("email or phone number already exists");        
            
            await _repo.AddAsync(user);

            await _unitOfWork.CommitAsync();

            return user;
    }

    
    public async Task<List<User>> GetAllAsync()
    {                   
            return await _repo.GetAllAsync();
    }

    public async Task<User> GetByEmailAsync(string email)
    {
        return await _repo.GetByEmailAsync(email);
    }

    public async Task<string> GenerateResetPasswordToken(User user)
    {
        TokenProvider tokenProvider = new TokenProvider(_configuration);
        string token = tokenProvider.CreatePasswordResetToken(user);
        
        //definir o tempo de expiração do token num config file 
        user.SetResetPasswordToken(token, DateTime.UtcNow.AddHours(24));
        await this._unitOfWork.CommitAsync();
        
        return token;

    }

    public async Task<string> SendEmailForAbusiveAccountAccess(string accountBlockedEmail)
    {   
        await _emailSender.SendEmailAsync("Account with username "+ accountBlockedEmail +" has been blocked due to abusive access / failing consequitive logins",( _configuration["Admin:Email"]),"Account Blocked");
        return "Email sent";
    }




    public async Task<string> ResetPassword (User user,string newPassword, string token)
    {

        if (!user.resetPasswordToken.resetPasswordToken.Equals( token))
        {
            throw new Exception("Invalid token");
        }

        //FALTA VERIFICAR SE O TOKEN AINDA É VALIDO EM TEMPO ?
        
        //DEFINIR O PASSWORD HASHER COM O AWAIT qnd tiver tempo
        PasswordHasher passwordHasher = new PasswordHasher();
        string newPasswordHash =  passwordHasher.HashPassword(newPassword);
        user.SetPassword(passwordHasher.HashPassword(newPassword));
        
        user.ClearResetPasswordToken();

        await this._unitOfWork.CommitAsync();

        return "Success"; ;

    }


    public async Task<string> sendEmailWithUrlResetPassword(string email, string url)
    {
        await _emailSender.SendEmailAsync($"Please reset your password by clicking here: <a href='{url}'>link</a>",email,"ResetPassword");
        
        return "Email sent";

    }


}


}