import { ComponentFixture, TestBed } from "@angular/core/testing";
import { ResetPasswordComponent } from "./reset-password.component";
import { ActivatedRoute, Router } from "@angular/router";
import { UserService } from "../../services/user.service";
import { of, throwError } from "rxjs";

describe('ResetPasswordComponent', () => {
  let component: ResetPasswordComponent;
  let fixture: ComponentFixture<ResetPasswordComponent>;
  let userService: jasmine.SpyObj<UserService>;
  let router: Router;
  let route: ActivatedRoute;

  beforeEach(async () => {
    const userServiceSpy = jasmine.createSpyObj('UserService', ['forgotPassword', 'resetPassword']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);
    const routeStub = {
      snapshot: {
        queryParamMap: {
          get: jasmine.createSpy('get').and.callFake((key: string) => {
            const params: { [key: string]: string } = { token: 'test-token', email: 'test@example.com' };
            return params[key];
          })
        }
      }
    };

    await TestBed.configureTestingModule({
      providers: [
        { provide: UserService, useValue: userServiceSpy },
        { provide: Router, useValue: routerSpy },
        { provide: ActivatedRoute, useValue: routeStub }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ResetPasswordComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;
    router = TestBed.inject(Router);
    route = TestBed.inject(ActivatedRoute);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with token and email from query params', () => {
    expect(component.token).toBe('test-token');
    expect(component.email).toBe('test@example.com');
    expect(component.resetPasswordStage).toBeTrue();
  });

  it('should request password reset', () => {
    userService.forgotPassword.and.returnValue(of({}));
    component.email = 'test@example.com';
    component.onResetPasswordRequest();
    expect(userService.forgotPassword).toHaveBeenCalledWith('test@example.com');
    expect(component.successMessage).toBe('Password reset email sent! Please check your email to reset your password.');
    expect(component.errorMessage).toBeNull();
  });

  it('should handle password reset request error', () => {
    userService.forgotPassword.and.returnValue(throwError({ error: { message: 'Error occurred' } }));
    component.email = 'test@example.com';
    component.onResetPasswordRequest();
    expect(userService.forgotPassword).toHaveBeenCalledWith('test@example.com');
    expect(component.successMessage).toBeNull();
    expect(component.errorMessage).toBe('Error occurred');
  });

  it('should reset password successfully', () => {
    userService.resetPassword.and.returnValue(of({}));
    component.newPassword = 'newPassword';
    component.confirmPassword = 'newPassword';
    component.token = 'test-token';
    component.email = 'test@example.com';
    component.onResetPassword();
    expect(userService.resetPassword).toHaveBeenCalledWith('test-token', 'newPassword', 'test@example.com');
    expect(component.successMessage).toBe('Password reset successfully!');
    expect(component.errorMessage).toBeNull();
    setTimeout(() => {
      expect(router.navigate).toHaveBeenCalledWith(['/login']);
    }, 1500);
  });

  it('should handle password reset error', () => {
    userService.resetPassword.and.returnValue(throwError({}));
    component.newPassword = 'newPassword';
    component.confirmPassword = 'newPassword';
    component.token = 'test-token';
    component.email = 'test@example.com';
    component.onResetPassword();
    expect(userService.resetPassword).toHaveBeenCalledWith('test-token', 'newPassword', 'test@example.com');
    expect(component.successMessage).toBeNull();
    expect(component.errorMessage).toBe('Error resetting password. Please try again.');
  });

  it('should show error if passwords do not match', () => {
    component.newPassword = 'newPassword';
    component.confirmPassword = 'differentPassword';
    component.onResetPassword();
    expect(component.errorMessage).toBe('Passwords do not match');
    expect(component.successMessage).toBeNull();
  });
});