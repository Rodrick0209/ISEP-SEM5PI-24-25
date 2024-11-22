import { ComponentFixture, TestBed } from '@angular/core/testing';
import { LoginComponent } from './login.component';
import { AuthService } from '../../services/auth.service';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { HttpClientModule } from '@angular/common/http';
import { RouterTestingModule } from '@angular/router/testing';

describe('LoginComponent', () => {
  let component: LoginComponent;
  let fixture: ComponentFixture<LoginComponent>;
  let authService: AuthService;
  let router: Router;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule, HttpClientModule, RouterTestingModule],
      providers: [
        {
          provide: AuthService,
          useValue: {
            login: jasmine.createSpy('login').and.returnValue(of({ token: 'test-token' })),
            verifyGoogleToken: jasmine.createSpy('verifyGoogleToken').and.returnValue(of({ token: { result: 'google-token' } })),
            saveToken: jasmine.createSpy('saveToken')
          }
        }
      ]
    }).compileComponents();

    (window as any).google = {
      accounts: {
        id: {
          initialize: jasmine.createSpy('initialize'),
          renderButton: jasmine.createSpy('renderButton'),
          prompt: jasmine.createSpy('prompt')
        }
      }
    };
    fixture = TestBed.createComponent(LoginComponent);
    component = fixture.componentInstance;
    authService = TestBed.inject(AuthService);
    router = TestBed.inject(Router);
    spyOn(router, 'navigate');
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call authService.login on onLogin', () => {
    component.email = 'test@example.com';
    component.password = 'password';
    component.onLogin();
    expect(authService.login).toHaveBeenCalledWith('test@example.com', 'password');
  });

  it('should navigate to /home on successful login', () => {
    component.onLogin();
    expect(authService.saveToken).toHaveBeenCalledWith('test-token');
    expect(router.navigate).toHaveBeenCalledWith(['/home']);
  });

  it('should set errorMessage on login failure', () => {
    (authService.login as jasmine.Spy).and.returnValue(throwError({ error: { message: 'Invalid credentials' } }));
    component.onLogin();
    expect(component.errorMessage).toBe('Erro no login: Invalid credentials');
  });

  it('should handle Google credential response', () => {
    const response = { credential: 'google-credential' };
    component.handleCredentialResponse(response);
    expect(authService.verifyGoogleToken).toHaveBeenCalled();
  });

  it('should navigate to /home on successful Google token verification', () => {
    const response = { credential: 'google-credential' };
    component.handleCredentialResponse(response);
    expect(authService.saveToken).toHaveBeenCalledWith('google-token');
    expect(router.navigate).toHaveBeenCalledWith(['/home']);
  });

  it('should set errorMessage on Google token verification failure', () => {
    (authService.verifyGoogleToken as jasmine.Spy).and.returnValue(throwError({ error: { message: 'Invalid Google token' } }));
    const response = { credential: 'google-credential' };
    component.handleCredentialResponse(response);
    expect(component.errorMessage).toBe('Error verifying Google token: Invalid Google token');
  });
});