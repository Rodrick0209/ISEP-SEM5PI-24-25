import { ComponentFixture, TestBed } from '@angular/core/testing';
import { HeaderComponent } from './header.component';
import { AuthService } from '../../services/auth.service';
import { Router } from '@angular/router';
import { of } from 'rxjs';


describe('HeaderComponent', () => {
  let component: HeaderComponent;
  let fixture: ComponentFixture<HeaderComponent>;
  let authServiceStub: Partial<AuthService>;
  let routerSpy = jasmine.createSpyObj('Router', ['navigate']);

  beforeEach(async () => {
    authServiceStub = {
      isLoggedIn$: of(true),
      clearToken: jasmine.createSpy('clearToken'),
      extractEmailFromToken: jasmine.createSpy('extractEmailFromToken').and.returnValue('test@example.com'),
      extractRoleFromToken: jasmine.createSpy('extractRoleFromToken').and.returnValue('admin')
    };

    await TestBed.configureTestingModule({
      imports: [ HeaderComponent ],
      providers: [
        { provide: AuthService, useValue: authServiceStub },
        { provide: Router, useValue: routerSpy }
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(HeaderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should set isLoggedIn to true on init', () => {
    component.ngOnInit();
    expect(component.isLoggedIn).toBeTrue();
  });

  it('should call clearToken and navigate to /home on logout', () => {
    component.logout();
    expect(authServiceStub.clearToken).toHaveBeenCalled();
    expect(routerSpy.navigate).toHaveBeenCalledWith(['/home']);
  });

  it('should return user email', () => {
    const email = component.getUserEmail();
    expect(email).toBe('test@example.com');
  });

  it('should return true if user is admin', () => {
    const isAdmin = component.isAdmin();
    expect(isAdmin).toBeTrue();
  });

  it('should return false if user is not doctor', () => {
    (authServiceStub.extractRoleFromToken as jasmine.Spy).and.returnValue('admin');
    const isDoctor = component.isDoctorUser();
    expect(isDoctor).toBeFalse();
  });

  it('should return false if user is not patient', () => {
    (authServiceStub.extractRoleFromToken as jasmine.Spy).and.returnValue('admin');
    const isPatient = component.isPatientUser();
    expect(isPatient).toBeFalse();
  });
});