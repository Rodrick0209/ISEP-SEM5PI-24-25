import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DeleteOperationRequestsComponent } from './delete-operation-requests.component';
import { ActivatedRoute, Router } from '@angular/router';
import { OperationRequestService } from '../../services/operationRequestService';
import { MessageService } from '../../services/message.service';
import { of, throwError } from 'rxjs';

describe('DeleteOperationRequestsComponent', () => {
    let component: DeleteOperationRequestsComponent;
    let fixture: ComponentFixture<DeleteOperationRequestsComponent>;
    let mockActivatedRoute: any;
    let mockOperationRequestService: any;
    let mockMessageService: any;
    let mockRouter: any;

    beforeEach(() => {
        mockActivatedRoute = {
            snapshot: {
                paramMap: {
                    get: jasmine.createSpy('get').and.returnValue('123')
                }
            }
        };

        mockOperationRequestService = {
            deleteOperationRequest: jasmine.createSpy('deleteOperationRequest').and.returnValue(of({}))
        };

        mockMessageService = {
            setMessage: jasmine.createSpy('setMessage')
        };

        mockRouter = {
            navigate: jasmine.createSpy('navigate')
        };

        TestBed.configureTestingModule({
            imports: [DeleteOperationRequestsComponent],
            providers: [
                { provide: ActivatedRoute, useValue: mockActivatedRoute },
                { provide: OperationRequestService, useValue: mockOperationRequestService },
                { provide: MessageService, useValue: mockMessageService },
                { provide: Router, useValue: mockRouter }
            ]
        }).compileComponents();

        fixture = TestBed.createComponent(DeleteOperationRequestsComponent);
        component = fixture.componentInstance;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });

    it('should initialize operationRequestId on ngOnInit', () => {
        component.ngOnInit();
        expect(component.operationRequestId).toBe('123');
    });

    it('should call deleteOperationRequest and navigate on successful delete', () => {
        component.operationRequestId = '123';
        component.onDelete();
        expect(mockOperationRequestService.deleteOperationRequest).toHaveBeenCalledWith('123');
        expect(mockMessageService.setMessage).toHaveBeenCalledWith('Operation request successfully deleted!');
        expect(mockRouter.navigate).toHaveBeenCalledWith(['/operationRequests']);
    });

    it('should set errorMessage on delete failure', () => {
        mockOperationRequestService.deleteOperationRequest.and.returnValue(throwError('error'));
        component.operationRequestId = '123';
        component.onDelete();
        expect(mockMessageService.setMessage).toHaveBeenCalledWith('Failed to delete operation request');
    });

    it('should navigate to /operationRequests on onCancel', () => {
        component.onCancel();
        expect(mockRouter.navigate).toHaveBeenCalledWith(['/operationRequests']);
    });
});